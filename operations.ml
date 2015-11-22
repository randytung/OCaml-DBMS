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

let where t r =
  [1;2;3]

(*returns a pair where fst is the first word of the string delimited by spaces
* and snd is the remaining string*)
let next_word (input:string) (del:char) : (string*string) =
  let input' = String.trim input in
  let spaceindex = (try (String.index input' del) with
  |Not_found -> String.length input'
  |_ -> String.index input' del) in
  let first = String.sub input' 0 spaceindex in
  let second = String.sub input' (spaceindex + 1)
    ((String.length input') - (String.length first + 1)) in
  let second' = String.trim second in
  (first,second')

let rec find_table db tab_name =
  match db with
  |[] -> failwith "table does not exist"
  |h::t-> if h.title = tab_name then h else find_table t tab_name

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
let delete (db:db) (commands : string) : db =
  let pretable = snd(next_word commands ' ') in
  let tab_name = fst(next_word pretable ' ') in
  let prewhere = snd(next_word pretable ' ') in
  let reqs = snd(next_word prewhere ' ') in
  let table = find_table db tab_name in
  let rows = where table reqs in
  let rec new_table db table  =
    (match db with
    |[] -> failwith "no table found"
    |h::t -> if h = table then
              (let rec hlpr cols acc nbr =
              match table.cols with
              |[] -> acc
              |h::t -> if (List.mem nbr rows) then hlpr t acc (nbr + 1)
                      else let newacc = acc @ [h] in hlpr t newacc (nbr + 1) in
              hlpr table.cols [] 0)
              else new_table t table) in
  let newtable = {title = table.title; cols = new_table db table} in
  let rec replace db table acc =
    (match db with
    |[] -> failwith "no table to replace :("
    |h::t -> if h = table then let newh = table in
            let newacc = acc @ [newh] in replace t table newacc
            else let newacc' = acc @ [h] in replace t table newacc') in
  replace db newtable []

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
let eval (db:db) (commands:string) : db =
  let commands' = next_word commands ' ' in
  match String.lowercase(fst commands') with
  |"delete" -> delete db (snd commands')
  |_ -> failwith "not a command"

(*takes an existing db and user input to evaluate commands and return an
* updated db*)
let revise_db (db:db) (input:string) : db =
  failwith "unimplemented"
