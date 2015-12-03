open Types

(** Builds a JSON file from the given db *)
val build : db -> string

(** Saves the JSON file to a given file location, overwrites old JSON *)
val save : db -> string -> unit