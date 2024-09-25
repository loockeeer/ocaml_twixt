type pos_t =
  { line : int
  ; col : int
  }

type player_t =
  | Black
  | Red
  | Unowned

type cell_t =
  { links : bool array
  ; mutable owner : player_t
  ; pos : pos_t
  }

type board_t =
  { size : int
  ; cells : cell_t array array
  ; mutable victory : bool * player_t
  }

let board_create size =
  let cells =
    Array.init_matrix size size (fun i j ->
      { links = Array.make 8 false; owner = Unowned; pos = { line = i; col = j } })
  in
  { cells; size; victory = false, Unowned }
;;

let allowed_movements =
  [| 2, 1; 1, 2; -1, 2; -2, 1; -2, -1; -1, -2; 1, -2; 2, -1 |]
  |> Array.map (fun x -> { line = fst x; col = snd x })
;;

let add_pos p1 p2 = { line = p1.line + p2.line; col = p1.col + p2.col }

let check_pos board p =
  let a, b = p.line, p.col in
  a >= 0
  && b >= 0
  && a < board.size
  && b < board.size
  && (not (a = 0 && b = 0))
  && (not (a = 0 && b = board.size - 1))
  && (not (a = board.size - 1 && b = 0))
  && not (a = board.size - 1 && b = board.size - 1)
;;

let get_near_cells board cell =
  let ret = Array.make 8 None in
  for i = 0 to 7 do
    if cell.links.(i)
    then (
      let new_pos = add_pos cell.pos allowed_movements.(i) in
      ret.(i)
      <- (if check_pos board new_pos
          then Some board.cells.(new_pos.line).(new_pos.col)
          else None))
  done;
  ret
;;

let get_adjacent_cells board cell =
  get_near_cells board cell
  |> Array.map (fun c ->
    match c with
    | Some w -> if cell.owner = w.owner then Some w else None
    | _ -> None)
;;

exception FastExit of player_t

let fast_dfs board player startcell =
  let visited = Array.make_matrix board.size board.size false in
  let rec aux v =
    visited.(v.pos.line).(v.pos.col) <- true;
    if player = Black && v.pos.col = board.size - 1
    then raise_notrace (FastExit player)
    else if player = Red && v.pos.line = board.size - 1
    then raise_notrace (FastExit player);
    Array.iter
      (fun c ->
        match c with
        | Some w -> if not visited.(w.pos.line).(w.pos.col) then ignore (aux w)
        | _ -> ())
      (get_adjacent_cells board v);
    false
  in
  try aux startcell with
  | FastExit _ -> true
;;

let check_victory board =
  try
    for i = 0 to board.size - 1 do
      let cellr = board.cells.(0).(i) in
      let cellb = board.cells.(i).(0) in
      if cellr.owner = Red && fast_dfs board Red cellr then raise_notrace (FastExit Red);
      if cellb.owner = Black && fast_dfs board Black cellb
      then raise_notrace (FastExit Black)
    done;
    false, Unowned
  with
  | FastExit x -> true, x
;;

let transfo i =
  match i with
  | 0 -> fun p -> fst p, snd p
  | 1 -> fun p -> -fst p, snd p
  | 2 -> fun p -> -snd p, fst p
  | 3 -> fun p -> -snd p, -fst p
  | 4 -> fun p -> -fst p, -snd p
  | 5 -> fun p -> fst p, -snd p
  | 6 -> fun p -> snd p, -fst p
  | 7 -> fun p -> snd p, fst p
  | _ -> failwith "undefined behavior"
;;

let index link_type =
  match link_type with
  | 2, 1 -> 0
  | 2, -1 -> 1
  | 1, -2 -> 2
  | -1, -2 -> 3
  | -2, -1 -> 4
  | -2, 1 -> 5
  | -1, 2 -> 6
  | 1, 2 -> 7
  | _ -> failwith "undefined behavior"
;;

let possible_collisions i =
  Array.map
    (fun x -> (transfo i) (fst x), index ((transfo i) (snd x)))
    [| (1, -1), (-1, 2)
     ; (1, 0), (-1, 2)
     ; (2, -1), (-1, 2)
     ; (2, 0), (-1, 2)
     ; (1, 0), (-2, 1)
     ; (2, 0), (-2, 1)
     ; (3, 0), (-2, 1)
     ; (0, -1), (1, 2)
     ; (1, 0), (1, 2)
    |]
;;

let check_collision board c1 i =
  let t = possible_collisions i in
  let x, y = c1.pos.line, c1.pos.col in
  let output = ref false in
  for n = 0 to 8 do
    let p, j = t.(n) in
    let c = board.cells.(x + fst p).(y + snd p) in
    if c.links.(j) then output := true
  done;
  !output
;;

let can_link board c1 c2 i =
  c1.owner = c2.owner
  && c1.owner <> Unowned
  && (not c1.links.(i))
  && not (check_collision board c1 i)
;;

let get_available_links board cell =
  get_near_cells board cell
  |> Array.mapi (fun i c ->
    match c with
    | Some w -> can_link board cell w i
    | _ -> false)
;;

let add_link board c1 i =
  let pos = add_pos c1.pos allowed_movements.(i) in
  let c2 = board.cells.(pos.line).(pos.col) in
  if can_link board c1 c2 i
  then (
    c1.links.(i) <- true;
    c2.links.((i + 4) mod 8) <- true;
    true)
  else false
;;

let remove_link board c1 i =
  let pos = add_pos c1.pos allowed_movements.(i) in
  let c2 = board.cells.(pos.line).(pos.col) in
  c1.links.(i) <- false;
  c2.links.((i + 4) mod 8) <- false
;;

let create_links board cell =
  let ret = Array.make 8 false in
  for i = 0 to 7 do
    ret.(i) <- add_link board cell i
  done;
  ret
;;

let place_player board pos player =
  if (not (check_pos board pos))
     || (player = Red && (pos.col = 0 || pos.col = board.size - 1))
     || (player = Black && (pos.line = 0 || pos.line = board.size - 1))
     || board.cells.(pos.line).(pos.col).owner <> Unowned
  then false
  else (
    board.cells.(pos.line).(pos.col).owner <- player;
    true)
;;
