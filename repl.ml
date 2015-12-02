open Operations
open Builder
open Types

let help () = ()

let rec read_lines acc =
  let s = read_line () in
  let new_acc =
    if acc = "" then s else acc^" "^s in
  if s.[String.length s - 1] = ';'
  then S.sub new_acc 0 (S.length new_acc - 1)
  else read_lines new_acc

(* prompts for user input, calls operations, saves the database using builder,
 * and recursively initializes itself with the revised db until user input
 * causes the loop to halt *)
let rec loop (db:db) (file_name:string) : unit =
  Printf.printf "Enter a command, or type \"help\" for a list of commands.\n";
  let input = read_lines "" in
  match String.lowercase input with
  | "help"  -> help(); loop db file_name
  | "quit"  -> save db file_name
  | _       -> (let new_db =
                    try (let x = revise_db db input in
                         print_string "Success!\n"; x)
                    with
                    | Failure msg -> print_string ("Error: " ^ msg ^
                                                   "\nPlease try again.\n"); db
                    | _ -> print_string "Error. Please try again.\n"; db in
                save new_db file_name;
                loop new_db file_name)