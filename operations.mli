open Types

(*returns a pair where fst is the first word of the string delimited by spaces
* and snd is the remaining string*)
val next_word : string -> (string * string)

(*returns a db restricted to the requirements given*)
val select : db -> string -> db

(*creates a new table with the given name*)
val create : db -> string -> db

(*inserts a row into a given table with its categories and corresponding values*)
val insert : db -> string -> db

(*updates a record in the table with new values matched with their corresponding
* categories*)
val update : db -> string -> (string * string) list -> string -> db

(*deletes a given row from a given table*)
val delete : db -> string -> db

(*deletes a given table*)
val drop : db -> string -> db

(*adds, removes, or modifies a given category from a given table*)
val alter : db -> string -> db

(*evaluates the commands given to it and returns an updated db*)
val eval : db -> string -> db

(*takes an existing db and user input to evaluate commands and return an
* updated db*)
val revise_db : db -> string -> db