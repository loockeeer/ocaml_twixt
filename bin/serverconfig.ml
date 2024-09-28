let _int_value name default =
  let tmp = Option.value (Sys.getenv_opt name) ~default:(string_of_int default) in
  try int_of_string tmp with
  | _ -> default
;;

let token_secret =
  Option.value
    (Sys.getenv_opt "TOKEN_SECRET")
    ~default:(string_of_int (Random.int Int.max_int))
;;

(* Cache related settings *)
let websocket_pool_hashtbl_size = _int_value "WEBSOCKET_POOL_HASHTBL_SIZE" 5
let cache_hashtbl_size = _int_value "CACHE_HASHTBL_SIZE" 5
let cache_ttl = _int_value "CACHE_TTL" 43200

(* Networking related settings *)
let listen_interface =
  Option.value (Sys.getenv_opt "LISTEN_INTERFACE") ~default:"localhost"
;;

let listen_port = _int_value "LISTEN_PORT" 8080

(* Elo related settings *)
let default_elo = _int_value "DEFAULT_ELO" 1000
let elo_coefficient = _int_value "ELO_COEFFICIENT" 20

(* Database related settings *)
let database_uri = Sys.getenv "DATABASE_URI"
