type outcome_t =
  | FirstWon
  | SecondWon
  | Draw

let win_prob r1 r2 = 1.0 /. (1. +. Float.pow 10. ((r1 -. r2) /. 400.))

let get_outcome_value outcome =
  match outcome with
  | FirstWon -> 1.
  | SecondWon -> 0.
  | Draw -> 0.5
;;

let calculate_elo oldr1 oldr2 k outcome =
  let p1 = win_prob oldr1 oldr2 in
  let p2 = win_prob oldr2 oldr1 in
  let ov = get_outcome_value outcome in
  oldr1 +. (k *. (ov -. p1)), oldr2 +. (k *. (1. -. ov -. p2))
;;
