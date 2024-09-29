open Ocaml_twixt_exchange

let send_error content =
  Types.{ error = content }
  |> Types.yojson_of_error_doc
  |> Yojson.Safe.to_string
  |> Dream.json ~status:`Bad_Request
;;

let json_receiver json_parser handler request =
  let%lwt body = Dream.body request in
  let parse =
    try Some (body |> Yojson.Safe.from_string |> json_parser) with
    | _ -> None
  in
  match parse with
  | Some doc -> handler doc request
  | None -> send_error "Received invalid JSON input"
;;

let login (pb : Types.login_request_doc) _ =
  match%lwt Store.User.login ~email:pb.email ~password:pb.password with
  | Ok (id, true) ->
    let sess_id = Store.generate_id () in
    let access, refresh =
      Token.SessionToken.create_token_pair
        ~access_ttl:1
        ~refresh_ttl:10
        ~sess_id
        ~user_id:id
        ~secret:Serverconfig.token_secret
        ~rotation:0
    in
    let%lwt _ = Store.Session.create ~user_id:id ~sess_id in
    Types.{ access_token = access; refresh_token = refresh }
    |> Types.yojson_of_login_response_doc
    |> Yojson.Safe.to_string
    |> Dream.json ~status:`OK
  | Ok (_, false) | Error _ -> send_error "bad credentials"
;;

let register _ = Dream.html "Good morning, world!"
let get_leaderboard _ = Dream.html "Good morning, world!"
let get_user_by_id _ = Dream.html "Good morning, world!"
let delete_self _ = Dream.html "Good morning, world!"
let get_game_list _ = Dream.html "Good morning, world!"

let websocket client (message : Wsmes.complete_message_t) =
  match message.data with
  | Wsmes.QBoard _ -> Dream.send client "hi"
  | Wsmes.QBoardHistory _ -> Dream.send client "hi"
  | Wsmes.QChatHistory _ -> Dream.send client "hi"
  | Wsmes.MChatSend (_, _) -> Dream.send client "hi"
  | Wsmes.MGameAction _ -> Dream.send client "hi"
  | Wsmes.MResign -> Dream.send client "hi"
  | Wsmes.MDraw -> Dream.send client "hi"
  | Wsmes.Subscribe _ -> Dream.send client "hi"
  | Wsmes.Unsubscibe _ -> Dream.send client "hi"
  | Wsmes.Auth token ->
    (match Token.SessionToken.parse_token token Serverconfig.token_secret with
     | Token.SessionToken.Malformed -> Dream.send client "malformed"
     | Token.SessionToken.Expired -> Dream.send client "expired"
     | Ok payload -> Dream.send client (string_of_int payload.rot))
  | _ -> Dream.send client "unexpected message"
;;
