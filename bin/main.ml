open Ocaml_twixt_lib

let game_cache : (Uuidm.t, (Uuidm.t*Uuidm.t*Twixt.board_t)) Cache.t = Cache.create Serverconfig.cache_ttl Serverconfig.cache_hashtbl_size

let clients : (int, Dream.websocket) Hashtbl.t =
  Hashtbl.create Serverconfig.websocket_pool_hashtbl_size

let track =
  let last_client_id = ref 0 in
  fun websocket ->
    last_client_id := !last_client_id + 1;
    Hashtbl.replace clients !last_client_id websocket;
    !last_client_id

let forget client_id =
  Hashtbl.remove clients client_id

let send message =
  Hashtbl.to_seq_values clients
  |> List.of_seq
  |> Lwt_list.iter_p (fun client -> Dream.send client message)

let handle_client client =
  let client_id = track client in
  let rec loop () =
    match%lwt Dream.receive client with
    | Some message ->
      let%lwt () = send message in
      loop ()
    | None ->
      forget client_id;
      Dream.close_websocket client
  in
  loop ()

let () =
  Dream.run ~interface:Serverconfig.listen_interface ~port:Serverconfig.listen_port
  @@ Dream.logger
  @@ Dream.router [
    Dream.post "/login" Handlers.login;
    Dream.post "/register" Handlers.register;
    Dream.get "/leaderboard" Handlers.get_leaderboard;
    Dream.get "/users/:id" Handlers.get_user_by_id;
    Dream.get "/games" Handlers.get_game_list;
    Dream.delete "/users" Handlers.delete_self;
    Dream.get "/websocket"
      (fun _ -> Dream.websocket handle_client);
  ]
