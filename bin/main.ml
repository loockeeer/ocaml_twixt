open Ocaml_twixt_exchange

let conn_manager = Handlers.ConnManager.create ()

let handle_client client =
  let client_id = Handlers.ConnManager.track conn_manager client in
  let rec loop () =
    match%lwt Dream.receive client with
    | Some message ->
      (try
         let mes = Wsmes.of_json message in
         let%lwt () = Handlers.websocket client (client_id, conn_manager) mes in
         loop ()
       with
       | Yojson.Json_error d ->
         let%lwt () =
           Wsmes.{ data = Wsmes.RNotOk d; id = None }
           |> Wsmes.json_of
           |> Dream.send client
         in
         loop ())
    | None ->
      Handlers.ConnManager.forget conn_manager client_id;
      Dream.close_websocket client
  in
  loop ()
;;

let () =
  Dream.run ~interface:Serverconfig.listen_interface ~port:Serverconfig.listen_port
  @@ Dream.logger
  @@ Dream.router
       [ Dream.post
           "/login"
           (Handlers.json_receiver Types.login_request_doc_of_yojson Handlers.login)
       ; Dream.post
           "/refresh"
           (Handlers.json_receiver Types.refresh_request_doc_of_yojson Handlers.refresh)
       ; Dream.post "/register" Handlers.register
       ; Dream.get "/leaderboard" Handlers.get_leaderboard
       ; Dream.get "/users/:id" Handlers.get_user_by_id
       ; Dream.get "/games" Handlers.get_game_list
       ; Dream.delete "/users" Handlers.delete_self
       ; Dream.get "/websocket" (fun _ -> Dream.websocket handle_client)
       ]
;;
