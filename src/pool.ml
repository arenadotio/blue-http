open Core_kernel
open Async_kernel
open Async_unix
module Unique_id = Unique_id.Int63 ()

let default_last_used_by = Unique_id.create ()

module Item = struct
  type 'a t =
    { value : 'a
    ; lock : unit Mvar.Read_write.t
    ; mutable is_dead : bool
    ; mutable last_used_by : Unique_id.t
    }

  (* This function creates a new Item which is already locked and must be unlocked when finished *)
  let make_locked value =
    { value; lock = Mvar.create (); is_dead = false; last_used_by = default_last_used_by }
  ;;

  let try_lock { lock; _ } = Mvar.take_now lock |> Option.is_some
  let unlocked { lock; _ } = Mvar.value_available lock
  let unlock { lock; _ } = Mvar.set lock ()
end

type 'a t =
  { items : 'a Item.t Unique_id.Table.t
  ; (* This sequencer is used to ensure that we don't have two async threads adding items at the same time, which
       would let them go over the [max_elements] limit. Note that we don't care if multiple threads delete items at
       the same time *)
    insert_lock : unit Sequencer.t
  ; (* This is used to notify waiting threads when items are added or deleted so they can update what they're
       waiting on *)
    mutated : unit Condition.t
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
  ; insert_lock = Sequencer.create ~continue_on_error:true ()
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

(* Note: You must be holding the lock to call this function *)
let kill_item { items; mutated; kill_item; on_empty; _ } ~key ({ Item.value; _ } as item) =
  if item.is_dead
  then Deferred.unit
  else (
    item.is_dead <- true;
    Hashtbl.remove items key;
    Condition.broadcast mutated ();
    Monitor.protect
      (fun () -> kill_item value)
      ~finally:(fun () ->
        (* Give any other processes using this pool a chance to add more items before cleaning up *)
        Scheduler.yield () >>| fun () -> if Hashtbl.is_empty items then on_empty ()))
;;

let close ({ items; _ } as t) =
  Hashtbl.to_alist items
  |> Deferred.List.iter ~how:`Parallel ~f:(fun (key, item) -> kill_item t ~key item)
;;

let enqueue
    ({ items
     ; name
     ; pool_id
     ; insert_lock
     ; mutated
     ; max_elements
     ; expire_timeout
     ; new_item
     ; check_item
     ; _
     } as t)
    f
  =
  let tags = [ "pool_name", name; "pool_id", Unique_id.to_string pool_id ] in
  Logger.with_tags tags
  @@ fun () ->
  Timing.run_with_timing ~tags ~label:"pool.enqueue"
  @@ fun () ->
  Deferred.repeat_until_finished ()
  @@ fun () ->
  let%bind item =
    (* Try to find an existing item that's not in-use *)
    Hashtbl.to_alist items
    |> List.find ~f:(fun (_, item) -> Item.try_lock item)
    |> function
    | Some (key, item) ->
      Logger.debug "Using available pool item %s" (Unique_id.to_string key);
      return (`Item_available (key, item))
    | None ->
      Throttle.enqueue insert_lock
      @@ fun () ->
      (* Add a new item if there's space in the queue and then use that *)
      if Hashtbl.length items < max_elements
      then (
        let key = Unique_id.create () in
        Logger.debug "Adding new item %s" (Unique_id.to_string key);
        let%map item = new_item () >>| Item.make_locked in
        Hashtbl.add_exn items ~key ~data:item;
        Condition.broadcast mutated ();
        `Item_available (key, item))
      else
        (* If the queue is full, wait for one of the existing items or for the size to change *)
        choice (Condition.wait mutated) (fun () ->
            Logger.debug "Starting over since pool was mutated";
            `Changed)
        :: (Hashtbl.to_alist items
           |> List.map ~f:(fun (key, item) ->
                  choice (Item.unlocked item) (fun () ->
                      (* We return `Changed instead of `Item available because [choice] doesn't guarantee that
                         only one of the deferreds finishes, and we only want to call [Item.try_lock] once *)
                      Logger.debug
                        ~tags
                        "Starting over since pool item %s is available"
                        (Unique_id.to_string key);
                      `Changed)))
        |> Deferred.choose
  in
  match item with
  | `Changed -> Deferred.return @@ `Repeat ()
  | `Item_available (key, item) ->
    Monitor.protect
      (fun () ->
        if item.is_dead
        then
          failwithf
            "Item %s was chosen from pool %s (%s) even though that item was dead. This \
             should be impossible because dead items are removed from the pool."
            (Unique_id.to_string key)
            name
            (Unique_id.to_string pool_id)
            ()
        else
          Monitor.try_with (fun () ->
              (* Update expiration last_used_by so other expiration processes don't delete this connection
                 while we're using it *)
              let unique_id = Unique_id.create () in
              item.last_used_by <- unique_id;
              if%bind check_item item.value >>| not
              then (
                Logger.debug
                  ~tags
                  "Check failed for %s, deleting and trying again"
                  (Unique_id.to_string key);
                kill_item t ~key item >>| fun () -> `Repeat ())
              else (
                Logger.debug "Starting job using item %s" (Unique_id.to_string key);
                let%map res = f item.value in
                Logger.debug "Job using item %s succeeded" (Unique_id.to_string key);
                (* Check expiration once the timeout finishes *)
                let expires = Time.(add (now ()) expire_timeout) in
                at expires
                >>= (fun () ->
                      (* If the uuid doesn't match then some other process has used this item since the expiration process started *)
                      if Unique_id.(item.last_used_by = unique_id)
                      then (
                        (* UUID matches so nothing else has used this item; time to delete it *)
                        Logger.debug
                          ~tags
                          "Pool item %s expired"
                          (Unique_id.to_string key);
                        kill_item t ~key item)
                      else Deferred.unit)
                |> don't_wait_for;
                `Finished res))
          >>= function
          | Ok res -> Deferred.return res
          | Error exn -> kill_item t ~key item >>| fun () -> raise exn)
      ~finally:(fun () ->
        (* We always unlock items even if they were killed, since we can get better error messages this way *)
        Item.unlock item;
        Deferred.unit)
;;
