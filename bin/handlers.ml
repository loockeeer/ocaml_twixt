open Ocaml_twixt_exchange

let login _ = Dream.html "cc"
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
