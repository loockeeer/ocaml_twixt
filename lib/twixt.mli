type pos_t
type player_t
type cell_t
type board_t

val check_victory : board_t -> bool * player_t
val get_adjacent_cells : board_t -> cell_t -> (cell_t option) array
val get_near_cells : board_t -> cell_t -> cell_t option array
val get_available_links : board_t -> cell_t -> bool array
val add_link : board_t -> cell_t -> int -> bool
val remove_link : board_t -> cell_t -> int -> unit
val create_links : board_t -> cell_t -> bool array
val place_player : board_t -> pos_t -> player_t -> bool
