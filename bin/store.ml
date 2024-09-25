module User = struct
  type t =
    { id : Uuidm.t
    ; username : string
    ; email : string
    ; password : string
    ; salt : string
    ; elo : int
    }

  let create = ()
end
