open Ocaml_twixt_lib
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type shortuser_t =
  { id : Uuidm.t
  ; username : string
  }

type userprofile_t =
  { id : Uuidm.t
  ; username : string
  ; elo : int
  }

type gamestatus_t =
  | Notstarted
  | Ongoing
  | Ended

type game_t =
  { id : Uuidm.t
  ; status : bool
  ; red : Uuidm.t
  ; black : Uuidm.t
  ; size : int
  ; journal : Journal.t
  }

type error_doc = { error : string } [@@deriving yojson]

type login_request_doc =
  { email : string
  ; password : string
  }
[@@deriving yojson]

type login_response_doc =
  { access_token : string
  ; refresh_token : string
  }
[@@deriving yojson]
