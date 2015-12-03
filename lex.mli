open Types

val lex : string -> string list -> [> `Col of bytes | `Op of op | `Val of value ] list