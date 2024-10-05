open Ocaml_twixt_lib
open Ocaml_twixt_exchange

type cached_game_t =
  { game_info : Store.Game.t
  ; board : Twixt.board_t
  }

type t = (Id.t, cached_game_t) Cache.t

let create = Cache.create

let fetch_game_opt (manager : t) id =
  match Cache.find_opt manager id with
  | Some x -> Lwt.return (Some x.data)
  | None ->
    (match%lwt Store.Game.fetch ~id with
     | Ok x ->
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
       Lwt.return (Some cg)
     | Error _ -> Lwt.return None)
;;
