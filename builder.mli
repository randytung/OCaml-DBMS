open Yojson.Basic.Util
open Types

(*builds a JSON file from the given db*)
val build : db -> Yojson.Basic.json

(*saves the JSON file to a given file location, overwrites old JSON*)
val save : db -> string -> unit


