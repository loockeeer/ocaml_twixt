open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type 'a corr =
  { id : Id.t option
  ; data : 'a
  }
[@@deriving yojson]

type messages_t =
  | QBoard
  | QChatHistory of Id.t option * int
  | MChatSend of string
  | MGameAction of string
  | MResign
  | MDraw
  | Subscribe of Id.t
  | Unsubscribe
  | Auth of string
  | RBoard of int * string
  | RChatHistory of (Id.t * Id.t * string) array
  | RBoardUpdate of string
  | ROk of string
  | RNotOk of string
  | DrawAsked
  | GameEnded of Types.Game.status_t
[@@deriving yojson]

type complete_message = messages_t corr [@@deriving yojson]

let json_of m = yojson_of_complete_message m |> Yojson.Safe.to_string
let of_json s = Yojson.Safe.from_string s |> complete_message_of_yojson
