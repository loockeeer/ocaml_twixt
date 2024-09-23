open Ocaml_twixt_lib

type 'a corr = {id: Uuidm.t; data: 'a}

type clt =
    | QBoard of Uuidm.t
    | QBoardHistory of Uuidm.t
    | QChatHistory of Uuidm.t
    | MChatSend of Uuidm.t * Buffer.t
    | MGameAction of Journal.entry_t
    | MResign
    | MDraw
    | Subscribe of Uuidm.t
    | Unsubscibe of Uuidm.t
type srv =
    | RBoard of Twixt.board_t
    | RBoardHistory of int * Journal.t
    | RChatHistory of (Types.shortuser_t * Buffer.t) array
    | RBoardUpdate of Journal.entry_t
    | SubOk
    | SubNotOk
type cltmsg = clt corr
type srvmsg = srv corr
