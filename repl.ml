open Operations
open Builder
open Types

let help () = ()

(* prompts for user input, calls operations, saves the database using builder,
 * and recursively initializes itself with the revised db until user input
 * causes the loop to halt *)
let rec loop (db:db) (file_name:string) : unit =
  Printf.printf "Enter a command, or type \"help\" for a list of commands.\n";
  let input = read_line() in
  match String.lowercase input with
  | "help"  -> help(); loop db file_name
  | "quit"  -> save db file_name
  | command -> (let new_db = revise_db db command in
                save new_db file_name;
                loop new_db file_name)