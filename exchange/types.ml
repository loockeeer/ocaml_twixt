open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module User = struct
  type short_t =
    { id : Id.t
    ; username : string
    }
  [@@deriving yojson]

  type t =
    { id : Id.t
    ; username : string
    ; elo : int
    }
  [@@deriving yojson]
end

module Game = struct
  type status_t =
    | BWin
    | RWin
    | Running
    | Waiting
  [@@deriving yojson]

  let int_of_status s =
    match s with
    | BWin -> 0
    | RWin -> 1
    | Running -> 2
    | Waiting -> 3
  ;;

  let status_of_int i =
    match i with
    | 0 -> BWin
    | 1 -> RWin
    | 2 -> Running
    | 3 -> Waiting
    | _ -> failwith "bad argument"
  ;;

  type t =
    { id : Id.t
    ; red : Id.t
    ; black : Id.t
    ; size : int
    ; repr : string
    ; status : status_t
    }
  [@@deriving yojson]
end

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

type refresh_request_doc = { refresh_token : string } [@@deriving yojson]
type refresh_response_doc = login_response_doc
