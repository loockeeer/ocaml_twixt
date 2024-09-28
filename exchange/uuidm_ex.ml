module Uuidm = struct
  include Uuidm

  let to_yojson id = `String (Uuidm.to_string id)

  let of_yojson x =
    match x with
    | `String content ->
      (match Uuidm.of_string content with
       | Some x -> Ok x
       | None -> Error "non")
    | _ -> Error "non"
  ;;
end
