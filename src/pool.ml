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
  ; mutation_sequencer : unit Sequencer.t
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
  ; mutation_sequencer = Sequencer.create ~continue_on_error:true ()
  ; max_elements
  ; expire_timeout
  ; new_item_f = new_item
  ; kill_item_f = kill_item
  ; check_item_f = check_item
  ; on_empty
  }
;;

let length { items; _ } = Hashtbl.length items

let close { items; mutation_sequencer; on_empty; _ } =
  Throttle.enqueue mutation_sequencer
  @@ fun () ->
  Hashtbl.data items
  |> Deferred.List.iter ~how:`Parallel ~f:(fun { value; _ } ->
         Throttle.kill value;
         Throttle.cleaned value)
  >>| fun () -> on_empty ()
;;

let rec enqueue
    ({ items
     ; mutation_sequencer
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
  (* Try to find an existing item that's not in-use *)
  let existing_item =
    Hashtbl.data items
    |> List.find ~f:(fun item -> Throttle.num_jobs_running item.value = 0)
  in
  let%bind item =
    Throttle.enqueue mutation_sequencer
    @@ fun () ->
    (* Add a new item if there's space in the queue and then use that *)
    match existing_item with
    | None when Hashtbl.length items < max_elements ->
      let%map value = new_item_f () >>| Sequencer.create ~continue_on_error:false in
      let item = { value; last_used_uuid = Uuid_unix.create () }
      and key = Uuid_unix.create () in
      (* If an exception occurs or if the item is deleted, remove it from the hashtable and call the user-given
         cleanup function *)
      Throttle.at_kill value (fun item ->
          Throttle.enqueue mutation_sequencer
          @@ fun () ->
          Hashtbl.remove items key;
          Monitor.protect
            (fun () -> kill_item_f item)
            ~finally:(fun () ->
              if Hashtbl.is_empty items then on_empty ();
              Deferred.unit));
      Hashtbl.add_exn items ~key ~data:item;
      item
    | _ ->
      Hashtbl.data items
      |> List.map ~f:(fun item ->
             choice (Throttle.capacity_available item.value) (Fn.const item))
      |> Deferred.choose
  in
  if Throttle.is_dead item.value
  then Throttle.cleaned item.value >>= fun () -> enqueue t f
  else if Throttle.num_jobs_running item.value > 0
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
      Throttle.cleaned item.value >>= fun () -> enqueue t f
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
