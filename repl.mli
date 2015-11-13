open Operations
open Builder
open Types

(*prompts for user input, calls operations, saves the database using builder, and
* recursively initializes itself with the revised db until user input causes the
* loop to halt*)
val loop : db -> string -> unit