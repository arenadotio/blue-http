open Core_kernel
open Async_kernel
open Async_unix
module Unique_id = Unique_id.Int63 ()

let default_last_used_by = Unique_id.create ()

module Item = struct
  type 'a t =
    { value : 'a Sequencer.t
    ; mutable last_used_by : Unique_id.t
    }

  let make value = { value; last_used_by = default_last_used_by }
end

type 'a t =
  { items : 'a Item.t Deferred.t Unique_id.Table.t
  ; mutated : unit Condition.t
  ; name : string
  ; pool_id : Unique_id.t
  ; max_elements : int
  ; expire_timeout : Time.Span.t
  ; new_item : unit -> 'a Deferred.t
  ; kill_item : 'a -> unit Deferred.t
  ; check_item : 'a -> bool Deferred.t
  ; on_empty : unit -> unit
  }

let create
    ~name
    ~max_elements
    ~expire_timeout
    ~new_item
    ~kill_item
    ?(on_empty = Fn.id)
    ~check_item
    ()
  =
  { items = Unique_id.Table.create ()
  ; mutated = Condition.create ()
  ; name
  ; pool_id = Unique_id.create ()
  ; max_elements
  ; expire_timeout
  ; new_item
  ; kill_item
  ; check_item
  ; on_empty
  }
;;

let length { items; _ } = Hashtbl.length items

let close { items; on_empty; _ } =
  Hashtbl.data items
  |> Deferred.List.iter ~how:`Parallel ~f:(fun item ->
         let%bind { value; _ } = item in
         Throttle.kill value;
         Throttle.cleaned value)
  >>| fun () -> on_empty ()
;;

let rec enqueue
    ({ items
     ; name
     ; pool_id
     ; mutated
     ; max_elements
     ; expire_timeout
     ; new_item
     ; kill_item
     ; check_item
     ; on_empty
     ; _
     } as t)
    f
  =
  let tags = [ "pool_name", name; "pool_id", Unique_id.to_string pool_id ] in
  let%bind item =
    (* Try to find an existing item that's not in-use *)
    Hashtbl.to_alist items
    |> Deferred.List.find_map ~f:(fun (key, item) ->
           let%map ({ value; _ } as item) = item in
           if Throttle.num_jobs_running value = 0 then Some (key, item) else None)
    >>= function
    | Some (key, item) ->
      Log.Global.debug ~tags "Using available pool item %s" (Unique_id.to_string key);
      return (`Item_available (key, item))
    | None ->
      (* Add a new item if there's space in the queue and then use that *)
      if Hashtbl.length items < max_elements
      then (
        let key = Unique_id.create () in
        Log.Global.debug ~tags "Adding new item %s" (Unique_id.to_string key);
        let item =
          let%map value = new_item () >>| Sequencer.create ~continue_on_error:false in
          (* If an exception occurs or if the item is deleted, remove it from the hashtable and call the user-given
         cleanup function *)
          Throttle.at_kill value (fun item ->
              Log.Global.debug ~tags "Removing item %s" (Unique_id.to_string key);
              Hashtbl.remove items key;
              Condition.broadcast mutated ();
              Monitor.protect
                (fun () -> kill_item item)
                ~finally:(fun () ->
                  (* Give any other processes using this pool a chance to add more items before cleaning up *)
                  Scheduler.yield ()
                  >>| fun () ->
                  if Hashtbl.is_empty items
                  then (
                    Log.Global.debug ~tags "Pool is empty, calling empty callback";
                    on_empty ())));
          Item.make value
        in
        Hashtbl.add_exn items ~key ~data:item;
        Condition.broadcast mutated ();
        let%map item = item in
        `Item_available (key, item))
      else
        (* If the queue is full, wait for one of the existing items or for the size to change *)
        choice (Condition.wait mutated) (fun () ->
            Log.Global.debug ~tags "Starting over since pool was mutated";
            `Mutated)
        :: (Hashtbl.to_alist items
           |> List.map ~f:(fun (key, item) ->
                  choice
                    (let%bind item = item in
                     Throttle.capacity_available item.value >>| fun () -> item)
                    (fun item ->
                      Log.Global.debug
                        ~tags
                        "Using newly available item %s"
                        (Unique_id.to_string key);
                      `Item_available (key, item))))
        |> Deferred.choose
  in
  match item with
  | `Mutated -> enqueue t f
  | `Item_available (key, item) ->
    if Throttle.is_dead item.value
    then (
      Log.Global.debug
        ~tags
        "Item %s has been killed, trying again"
        (Unique_id.to_string key);
      Throttle.cleaned item.value >>= fun () -> enqueue t f)
    else if Throttle.num_jobs_running item.value > 0
    then (
      Log.Global.debug
        ~tags
        "Item %s has another job running, trying again"
        (Unique_id.to_string key);
      enqueue t f)
    else (
      Log.Global.debug ~tags "Enqueueing job for item %s" (Unique_id.to_string key);
      (* Update expiration last_used_by so other expiration processes don't delete this connection
         while we're using it *)
      let unique_id = Unique_id.create () in
      item.last_used_by <- unique_id;
      let%bind res =
        Throttle.enqueue item.value (fun value ->
            if%bind check_item value
            then (
              let%map res = f value in
              `Ok res)
            else return `Check_failed)
      in
      match res with
      | `Check_failed ->
        Log.Global.debug
          ~tags
          "Check failed for %s, deleting and trying again"
          (Unique_id.to_string key);
        Throttle.kill item.value;
        Throttle.cleaned item.value >>= fun () -> enqueue t f
      | `Ok res ->
        Log.Global.debug ~tags "Job for %s succeeded" (Unique_id.to_string key);
        (* Check expiration once the timeout finishes *)
        let expires = Time.(add (now ()) expire_timeout) in
        upon (at expires) (fun () ->
            (* If the uuid doesn't match then some other process has used this item since the expiration process started *)
            if Unique_id.(item.last_used_by = unique_id)
            then (
              (* UUID matches so nothing else has used this item; time to delete it *)
              Log.Global.debug ~tags "Pool item %s expired" (Unique_id.to_string key);
              Throttle.kill item.value));
        return res)
;;
