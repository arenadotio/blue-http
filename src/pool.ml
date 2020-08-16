open Core_kernel
open Async_kernel
open Async_unix

type 'a item =
  { value : 'a Sequencer.t
  ; (* TODO: We need this ID to be unique per-process but these UUID's are probably overkill *)
    mutable last_used_uuid : Uuid.t
  }

type 'a t =
  { items : 'a item Uuid.Table.t
  ; max_elements : int
  ; expire_timeout : Time.Span.t
  ; new_item_f : unit -> 'a Deferred.t
  ; kill_item_f : 'a -> unit Deferred.t
  ; check_item_f : 'a -> bool Deferred.t
  ; on_empty : unit -> unit
  }

let create
    ~max_elements
    ~expire_timeout
    ~new_item
    ~kill_item
    ?(on_empty = Fn.id)
    ~check_item
    ()
  =
  { items = Uuid.Table.create ()
  ; max_elements
  ; expire_timeout
  ; new_item_f = new_item
  ; kill_item_f = kill_item
  ; check_item_f = check_item
  ; on_empty
  }
;;

let close { items; on_empty; _ } =
  Hashtbl.data items
  |> Deferred.List.iter ~how:`Parallel ~f:(fun { value; _ } ->
         Throttle.kill value;
         Throttle.cleaned value)
  >>| fun () -> on_empty ()
;;

let rec enqueue
    ({ items
     ; max_elements
     ; expire_timeout
     ; new_item_f
     ; kill_item_f
     ; check_item_f
     ; on_empty
     ; _
     } as t)
    f
  =
  let%bind item =
    (* First try to find an unused pre-built item *)
    Hashtbl.data items
    |> List.find ~f:(fun { value; _ } -> Throttle.num_jobs_running value = 0)
    |> function
    | Some item -> return item
    | None ->
      (* Add a new item if there's space in the queue and then use that *)
      if Hashtbl.length items < max_elements
      then (
        let%map value = new_item_f () >>| Sequencer.create ~continue_on_error:false in
        let item = { value; last_used_uuid = Uuid_unix.create () }
        and key = Uuid_unix.create () in
        (* If an exception occurs or if the item is deleted, remove it from the hashtable and call the user-given
         cleanup function *)
        Throttle.at_kill value (fun item ->
            kill_item_f item
            >>| fun () ->
            Hashtbl.remove items key;
            if Hashtbl.is_empty items then on_empty ());
        Hashtbl.add_exn items ~key ~data:item;
        item)
      else (
        (* Wait for an item in the queue *)
        let%map item =
          Hashtbl.data items
          |> List.map ~f:(fun item ->
                 choice (Throttle.prior_jobs_done item.value) (Fn.const item))
          |> Deferred.choose
        in
        item)
  in
  if Throttle.is_dead item.value || Throttle.num_jobs_running item.value > 0
  then enqueue t f
  else (
    (* Update expiration UUID so other expiration processes don't delete this connection
       while we're using it *)
    let uuid = Uuid_unix.create () in
    item.last_used_uuid <- uuid;
    let%bind res =
      Throttle.enqueue item.value (fun value ->
          if%bind check_item_f value
          then (
            let%map res = f value in
            `Ok res)
          else return `Check_failed)
    in
    match res with
    | `Check_failed ->
      Throttle.kill item.value;
      enqueue t f
    | `Ok res ->
      (* Check expiration once the timeout finishes *)
      let expires = Time.(add (now ()) expire_timeout) in
      upon (at expires) (fun () ->
          (* If the uuid doesn't match then some other process has used this item since the expiration process started *)
          if Uuid.(item.last_used_uuid = uuid)
          then (
            (* UUID matches so nothing else has used this item; time to delete it *)
            Log.Global.debug "Pool item expired";
            Throttle.kill item.value));
      return res)
;;
