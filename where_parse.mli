open Types

(* convert [tokens] from lexer to the indices of matched values in [cols] *)
val parse : column list -> [> `Col of bytes | `Op of op | `Val of value ] list -> int list