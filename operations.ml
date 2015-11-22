open Types

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
  {title = tab_name; cats = []}::db

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
  | h::t -> let {title = s; cats = _} = h in
            if tab_name = s then t else h::(drop t tab_name)

(*adds, removes, or modifies a given category from a given table*)
let alter (db:db) (cat_name:string) (command:string) : db =
  failwith "unimplemented"

(*evaluates the commands given to it and returns an updated db*)
let eval (db:db) (commands:string list) : db =
  failwith "unimplemented"

(*takes an existing db and user input to evaluate commands and return an
* updated db*)
let revise_db (db:db) (input:string) : db =
  failwith "unimplemented"
