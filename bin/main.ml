open Ocaml_twixt_lib
open Ocaml_twixt_exchange

let game_manager =
  Gamemanager.create Serverconfig.cache_ttl Serverconfig.cache_hashtbl_size
;;

type client_track_t =
  { sess_id : Uuidm.t
  ; expiry : int
  }

let clients : (int, client_track_t option * Dream.websocket) Hashtbl.t =
  Hashtbl.create Serverconfig.websocket_pool_hashtbl_size
;;

let track =
  let last_client_id = ref 0 in
  fun websocket ->
    last_client_id := !last_client_id + 1;
    Hashtbl.replace clients !last_client_id (None, websocket);
    !last_client_id
;;

let forget client_id = Hashtbl.remove clients client_id

let handle_client client =
  let client_id = track client in
  let rec loop () =
    match%lwt Dream.receive client with
    | Some message ->
      (match Wsmes.parse message with
       | Some mes ->
         let%lwt () = Handlers.websocket client mes in
         loop ()
       | None ->
         let%lwt () = Dream.send client "wtf" in
         loop ())
    | None ->
      forget client_id;
      Dream.close_websocket client
  in
  loop ()
;;

let () =
  Dream.run ~interface:Serverconfig.listen_interface ~port:Serverconfig.listen_port
  @@ Dream.logger
  @@ Dream.router
       [ Dream.post "/login" Handlers.login
       ; Dream.post "/register" Handlers.register
       ; Dream.get "/leaderboard" Handlers.get_leaderboard
       ; Dream.get "/users/:id" Handlers.get_user_by_id
       ; Dream.get "/games" Handlers.get_game_list
       ; Dream.delete "/users" Handlers.delete_self
       ; Dream.get "/websocket" (fun _ -> Dream.websocket handle_client)
       ]
;;
