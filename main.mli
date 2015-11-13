open Parser
open Repl
open Types

(*prompts for a file to import, calls parser to create a db, and initializes
* the REPL using the db and filename*)
val main : unit -> unit