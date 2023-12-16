type t = private (bool * string) list

exception Non_consecutive
exception Invalid

val parse : (char * string) list -> char -> t
val correct : (bool * string) list -> int
