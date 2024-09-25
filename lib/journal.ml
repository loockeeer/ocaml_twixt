open Twixt

type entry_t =
  | AddPeg of player_t * pos_t
  | Victory of player_t
  | AddLink of pos_t * int
  | RemoveLink of pos_t * int

type t = entry_t array

let interpret_entry board entry =
  match entry with
  | AddPeg (player, pos) -> place_player board pos player |> ignore
  | AddLink (pos1, idx) -> add_link board board.cells.(pos1.line).(pos1.col) idx |> ignore
  | RemoveLink (pos1, idx) -> remove_link board board.cells.(pos1.line).(pos1.col) idx
  | Victory player -> board.victory <- true, player
;;

let recreate_board size journal i =
  if i < 0 || i >= Array.length journal
  then board_create size
  else (
    let board = board_create size in
    for j = 0 to i - 1 do
      interpret_entry board journal.(j)
    done;
    board)
;;
