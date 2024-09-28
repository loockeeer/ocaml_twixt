open Ocaml_twixt_lib

type 'a corr =
  { id : Uuidm.t option
  ; data : 'a
  }

type messages_t =
  | QBoard of Uuidm.t
  | QBoardHistory of Uuidm.t
  | QChatHistory of Uuidm.t
  | MChatSend of Uuidm.t * string
  | MGameAction of Journal.entry_t
  | MResign
  | MDraw
  | Subscribe of Uuidm.t
  | Unsubscibe of Uuidm.t
  | Auth of string
  | RBoard of Twixt.board_t
  | RBoardHistory of int * Journal.t
  | RChatHistory of (Types.shortuser_t * Buffer.t) array
  | RBoardUpdate of Journal.entry_t
  | ROk
  | RNotOk
  | DrawAsked

type complete_message_t = messages_t corr

val parse : string -> complete_message_t option
