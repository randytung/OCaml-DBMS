open Yojson.Basic.Util
open Parser
open Repl
open Types

(* gets input and turns into lowercase *)
let get_input prompt =
  print_string prompt; String.lowercase (read_line())

(* repeats prompt for input until valid response - yes or no to existing db *)
let rec db_exists () =
  let input = get_input "Do you have an existing database? [y/n] \n" in
  match input with
  | "y" -> true
  | "n" -> false
  | _ -> (print_string "Invalid response.\n"; db_exists())

(* repeats prompt for input until a valid db can be created *)
let rec get_db prompt again =
  let file_name = get_input prompt in
  if String.sub file_name ((String.length file_name) - 5) 5 <> ".json"
  then (print_string "Invalid response.\n"; get_db prompt again)
  else if not again then ([], file_name)
  else
    let json = try Some (Yojson.Basic.from_file file_name) with _ -> None in
    match json with
    | None -> (print_string "File does not exist.\n"; get_db prompt again)
    | Some j -> let db = (try `Db (create_db j) with
                          | Failure msg -> `Fail ("JSON Parse Error: " ^
                                                  msg ^ "\n")
                          | _ -> `Fail "JSON does not comply with schema.\n") in
                match db with
                | `Db db -> (create_db j, file_name)
                | `Fail msg -> (print_string msg; get_db prompt again)

(* makes db from file if [again], otherwise makes empty db *)
let create again =
  let prompt =
    let first =
      if again then "Please enter the file name of your database.\n"
      else "What would you like to name your database?\n" in
    first ^ "[Name must be of the format \"[db_name].json\"]\n" in
  get_db prompt again

(* prompts for a file to import, calls parser to create a db, and initializes
 * the REPL using the db and filename *)
let main () : unit =
  let (db, file_name) = create (db_exists()) in
  loop db file_name

let _ = main()