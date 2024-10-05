type t

val compare : t -> t -> int
val yojson_of_t : t -> [> `String of string ]
val t_of_yojson : [> `String of string ] -> t
val generate : unit -> t
val of_string : ?pos:int -> string -> t option
val to_string : ?upper:bool -> t -> string
