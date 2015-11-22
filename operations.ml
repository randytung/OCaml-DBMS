open Types


let rec add_nulls (values:value list) : value list =
  match values with
  |[] -> []
  |h::t -> VNull::add_nulls t

let rec delete_column (column_list: column list) (column_name: string) : column list=
  match column_list with
  |[] -> []
  |h::t -> if h.name = column_name
              then delete_column t column_name
            else
              h::delete_column(t) (column_name)

let rec modify_column (column_list: column list) (column_name: string)
  (original_name:string): column list =
  match column_list with
  |[] -> []
  |h::t -> if h.name = original_name then
              {h with name = column_name}::t
           else
              h::modify_column (t) (column_name) (original_name)


(*returns a pair where fst is the first word of the string delimited by spaces
* and snd is the remaining string*)
let next_word (input:string) : (string*string) =
  let input' = String.trim input in
  let spaceindex = (try (String.index input' (' ')) with
  |Not_found -> String.length input'
  |_ -> String.index input' (' ')) in
    let first = String.sub input' 0 spaceindex in
    let second = String.sub input' spaceindex
      ((String.length input') - (String.length first)) in
    let second' = String.trim second in
    (first,second')

(*returns a db restricted to the requirements given*)
let select (db:db) (reqs:string) : db =
  failwith "unimplemented"

(*creates a new table with the given name*)
let create (db:db) (tab_name:string) : db =
  {title = tab_name; cols = []}::db

(*inserts a row into a given table with its categories and corresponding values*)
let insert (db:db) (tab_name:string) (cat_names:string list)
           (rows:string list) : db =
  failwith "unimplemented"

(*updates a record in the table with new values matched with their corresponding
* categories*)
(* [new_vals] is in the form [("cat_name", "new_val_at_the_row")]

   i.e.
   UPDATE Customers
   SET ContactName = "Alfred", City="Hamburg"
   WHERE CustomerName = "Alfred 2.0" *)

let update (db:db) (tab_name:string) (new_vals:(string * string) list)
           (reqs:string) : db =
  failwith "unimplemented"

(*deletes a given row from a given table*)
let delete (db:db) (tab_name:string) (reqs:string) : db =
  failwith "unimplemented"

(*deletes a given table*)
let rec drop (db:db) (tab_name:string) : db =
  match db with
  | [] -> failwith "Table not in database"
  | h::t -> let {title = s; cols = _} = h in
            if tab_name = s then t else h::(drop t tab_name)

(*adds, removes, or modifies a given category from a given table*)
let alter (db:db) (cat_name:string) (command:string) : db =
  let (table_name,next_commands) = next_word command in
  let (alter_command,next_commands) = next_word next_commands in
  let (column_name,next_commands) = next_word next_commands in
  let (modify_column_name,next_commands) = next_word next_commands in
  let find_column_and_alter (column_list :column list) (column_name:string)
    (alter_command : string) (modify_column_name:string): column list =
    if alter_command = "add" then let new_column = add_nulls (List.hd (column_list)).vals in
      {name = column_name; vals = new_column; typ = TInt}::column_list
    else if alter_command = "delete" then delete_column column_list column_name
    else if alter_command = "modify" then
      modify_column column_list column_name modify_column_name
    else
      failwith "Not a correct command" in
  let rec do_command (db:db) (table_name:string) (column_name:string)
    (alter_command:string) (modify_column_name:string) : db =
    match db with
    |[] -> failwith "Table does not exist"
    |h::t -> if h.title = table_name then
              let columns = find_column_and_alter h.cols column_name alter_command modify_column_name in
            {h with cols = columns}::t
          else
              h::(do_command (t) (table_name) (column_name) (alter_command) (modify_column_name)) in
  do_command db table_name column_name alter_command modify_column_name


(*evaluates the commands given to it and returns an updated db*)
let eval (db:db) (commands:string list) : db =
  failwith "unimplemented"

(*takes an existing db and user input to evaluate commands and return an
* updated db*)
let revise_db (db:db) (input:string) : db =
  failwith "unimplemented"
