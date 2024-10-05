include Uuidm

let generate = v4_gen (Random.State.make_self_init ())
let yojson_of_t id = `String (Uuidm.to_string id)

let t_of_yojson x =
  match x with
  | `String content ->
    (match Uuidm.of_string content with
     | Some x -> x
     | None -> failwith "non")
  | _ -> failwith "non"
;;
