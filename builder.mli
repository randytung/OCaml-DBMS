open Yojson

(*builds a JSON file from the given db*)
val build : db -> Yojson.Basic.Json

(*saves the JSON file to a given file location, overwrites old JSON*)
val save : db -> string -> unit


