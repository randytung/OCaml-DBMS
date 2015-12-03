open Types

(** returns association list between a column name and new values of each column
 *  in the set of columns converted from [cols] using [tokens] from the lexer *)
val parse : column list -> [> `Col of bytes | `Op of op | `Val of value ] list -> (bytes * value list) list