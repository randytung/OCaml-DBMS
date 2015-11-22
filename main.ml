open Yojson.Basic.Util
open Parser
open Repl
open Types

(* gets input and turns into lowercase *)
let get_input prompt =
  Printf.printf "%s" prompt; String.lowercase (read_line())

(* repeats prompt for input until valid response - yes or no to existing db *)
let rec db_exists () =
  let input = get_input "Do you have an existing database? [y/n] \n" in
  match input with
  | "y" -> true
  | "n" -> false
  | _ -> (Printf.printf "Invalid response.\n"; db_exists())

(* repeats prompt for input until it is a valid json filename *)
let rec get_filename prompt =
  let input = get_input prompt in
  if String.sub input ((String.length input) - 4) 4 = ".json" then input
  else (Printf.printf "Invalid response.\n"; get_filename prompt)

(* makes db from file if [again], otherwise makes empty db *)
let create again =
  let prompt =
    let first =
      if again then "Please enter the file name of your database.\n"
      else "What would you like to name your database?\n" in
    first ^ "Name must be of the format \"[db_name].json\"" in
  let file_name = get_filename prompt in
  if again then
    let json = Yojson.Basic.from_file file_name in
    (create_db json, file_name)
  else
    ([], file_name)

(* prompts for a file to import, calls parser to create a db, and initializes
 * the REPL using the db and filename *)
let main () : unit =
  let (db, file_name) = create (db_exists()) in
  loop db file_name

let _ = main()