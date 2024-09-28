open Ocaml_twixt_lib

type cached_game_t =
  { game_info : Store.Game.t
  ; board : Twixt.board_t
  }

type t = (Uuidm.t, cached_game_t) Cache.t

let create = Cache.create

let fetch_game_opt (manager : t) id =
  match Cache.find_opt manager id with
  | Some x -> Some x.data
  | None ->
    (match Store.Game.fetch id with
     | Some x ->
       let cg =
         { game_info = x
         ; board =
             Journal.recreate_board
               x.size
               (Option.value (Journal.of_string x.repr) ~default:[||])
               (-1)
         }
       in
       Cache.encache manager id cg;
       Some cg
     | None -> None)
;;
