let websocket_pool_hashtbl_size = 
    let tmp = Option.value (Sys.getenv_opt "WEBSOCKET_POOL_HASHTBL_SIZE") ~default:"5" in
    try int_of_string tmp with _ -> 5

let cache_hashtbl_size =
    let tmp = Option.value (Sys.getenv_opt "CACHE_HASHTBL_SIZE") ~default:"5" in
    try int_of_string tmp with _ -> 5

let listen_interface = Option.value (Sys.getenv_opt "LISTEN_INTERFACE") ~default:"localhost"

let listen_port =
    let tmp = Option.value (Sys.getenv_opt "LISTEN_PORT") ~default:"8080" in
    try int_of_string tmp with _ -> 8080

let cache_ttl =
    let tmp = Option.value (Sys.getenv_opt "CACHE_TTL") ~default:"43200" in
    try int_of_string tmp with _ -> 43200

