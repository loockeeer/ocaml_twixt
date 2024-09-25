module Duration = struct
  type t =
    | Seconds
    | Minutes
    | Hours
    | Days

  let ( $: ) qty dt =
    match dt with
    | Seconds -> qty
    | Minutes -> qty * 60
    | Hours -> qty * 60 * 60
    | Days -> qty * 60 * 60 * 24
  ;;
end

type 'a payload_t =
  { last_access : int
  ; data : 'a
  }

type ('id, 'dt) t = int * ('id, 'dt payload_t) Hashtbl.t

let int_time () =
  let t = Unix.time () in
  int_of_float (Float.round t)
;;

let create size ttl = ttl, Hashtbl.create size

let encache (_, cache) id data =
  Hashtbl.replace cache id { last_access = int_time (); data }
;;

let decache (_, cache) id = Hashtbl.remove cache id

let sync (ttl, cache) =
  Hashtbl.filter_map_inplace
    (fun _ payload ->
      if payload.last_access + ttl <= int_time () then None else Some payload)
    cache
;;
