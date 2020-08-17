open Core_kernel
open Async_kernel
open Async_unix

module Unique_id = struct
  module T = struct
    type t = int64 [@@deriving compare, hash, sexp]
  end

  include T
  module Table = Hashtbl.Make (T)

  (* Even if we incremented this counter every nanosecond, we could generate unique ID's for over 500 years
     https://www.wolframalpha.com/input/?i=%282%5E64+-+1%29+*+1+ns *)
  let counter = ref Int64.min_value

  let make () =
    let t = !counter in
    Int64.incr counter;
    t
  ;;

  let ( = ) a b = compare a b = 0
end

let default_last_used_by = Unique_id.make ()

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
  ; max_elements : int
  ; expire_timeout : Time.Span.t
  ; new_item : unit -> 'a Deferred.t
  ; kill_item : 'a -> unit Deferred.t
  ; check_item : 'a -> bool Deferred.t
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
  { items = Unique_id.Table.create ()
  ; mutated = Condition.create ()
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
  let%bind item =
    (* Try to find an existing item that's not in-use *)
    Hashtbl.data items
    |> Deferred.List.find_map ~f:(fun item ->
           let%map ({ value; _ } as item) = item in
           if Throttle.num_jobs_running value = 0 then Some item else None)
    >>= function
    | Some item -> return (`Item_available (item, `Reuse))
    | None ->
      (* Add a new item if there's space in the queue and then use that *)
      if Hashtbl.length items < max_elements
      then (
        let key = Unique_id.make () in
        let item =
          let%map value = new_item () >>| Sequencer.create ~continue_on_error:false in
          (* If an exception occurs or if the item is deleted, remove it from the hashtable and call the user-given
         cleanup function *)
          Throttle.at_kill value (fun item ->
              Hashtbl.remove items key;
              Condition.broadcast mutated ();
              Monitor.protect
                (fun () -> kill_item item)
                ~finally:(fun () ->
                  (* Give any other processes using this pool a chance to add more items before cleaning up *)
                  Scheduler.yield ()
                  >>| fun () -> if Hashtbl.is_empty items then on_empty ()));
          Item.make value
        in
        Hashtbl.add_exn items ~key ~data:item;
        Condition.broadcast mutated ();
        let%map item = item in
        `Item_available (item, `New))
      else
        (* If the queue is full, wait for one of the existing items or for the size to change *)
        choice (Condition.wait mutated) (fun () -> `Mutated)
        :: (Hashtbl.data items
           |> List.map ~f:(fun item ->
                  choice
                    (let%bind item = item in
                     Throttle.capacity_available item.value >>| fun () -> item)
                    (fun item -> `Item_available (item, `Reuse))))
        |> Deferred.choose
  in
  match item with
  | `Mutated -> enqueue t f
  | `Item_available (item, new_or_reused) ->
    if Throttle.is_dead item.value
    then Throttle.cleaned item.value >>= fun () -> enqueue t f
    else if Throttle.num_jobs_running item.value > 0
    then enqueue t f
    else (
      (* Update expiration last_used_by so other expiration processes don't delete this connection
         while we're using it *)
      let unique_id = Unique_id.make () in
      item.last_used_by <- unique_id;
      let%bind res =
        Throttle.enqueue item.value (fun value ->
            if%bind check_item value
            then (
              let%map res =
                f
                  ~is_new:
                    (match new_or_reused with
                    | `New -> true
                    | `Reuse -> false)
                  value
              in
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
            if Unique_id.(item.last_used_by = unique_id)
            then (
              (* UUID matches so nothing else has used this item; time to delete it *)
              Log.Global.debug "Pool item expired";
              Throttle.kill item.value));
        return res)
;;
