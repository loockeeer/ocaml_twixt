open Ocaml_twixt_lib

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
