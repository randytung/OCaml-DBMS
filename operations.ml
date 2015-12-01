open Types
open String

module S = String


(**********************)
(*  Helper Functions  *)
(**********************)


(******** PARSING FUNCTIONS ********)

(* returns a pair [(word, rest)] where [word] is the input string up to but
 * not including the first instance of del, trimmed to remove extra spaces.
 * [rest] is the rest of the input string, with the first delimiter removed
 * and not trimmed. [rest] is an option, because if [input] had no dels,
 * the whole string is returned in [word] and None is returned in [rest].*)
let next_chunk raw_input del =
  let input = S.trim raw_input in
  let loc = try Some (S.index input del) with _ -> None in
  match loc with
  | None   -> (input, None)
  | Some i -> let first = S.sub input 0 i in
              let second = S.sub input (i+1) ((S.length input) - (i+1)) in
              (S.trim first, Some second)

(* returns the next word of a str (split by spaces) as fst, then for snd either
 * the the rest of the string or the empty string *)
let next_word s =
  match next_chunk s ' ' with
  | (a, Some b) -> (a, b)
  | (a, None)   -> (a, "")

let next_word_comma s =
   match next_chunk s ',' with
  | (a, Some b) -> (a, b)
  | (a, None)   -> (a, "")

let next_word_quote s =
  match next_chunk s '"' with
  | (a, Some b) -> (a, b)
  | (a, None)   -> (a, "")


(* removes a redundant word from a string before performing function [f].
 * fails if the redundant word wasn't what was expected. *)
let extra_word word f str =
  let (test, rest) = next_word str in
  if word = S.lowercase test then f rest else failwith "not a command"

(* returns a list of chunks that make up [input] delimited by [del].
 * each element of the list is trimmed to remove extra spaces. *)
let rec list_chunks input del =
  let (h,t) = next_chunk input del in
  match t with
  | None -> [h]
  | Some s -> h::(list_chunks s del)

(* returns if [sub] is a substring of [str]
 * if [sub] is a substring, also return position of its first occurrence
 * if [sub] is not a substring, returns -1 *)
let rec is_substring str sub pos=
  if pos + S.length sub > S.length str then (false, -1)
  else
    if S.sub str pos (S.length sub) = sub then (true, pos)
    else
      is_substring str sub (pos+1)

(* returns a pair of (column_names, values) that make up [input] delimited by [del].
 * each element of the list is trimmed to remove extra spaces. *)
let rec list_cols_vals input del : (string list * string) =
  let (h,t) = next_chunk input del in
  let subs = is_substring h "values" 0 in
  match h, t, fst subs with
  | s1, Some s2, true ->
      let fst_val = S.trim (S.sub s1 (snd subs + 6) (S.length s1 - (snd subs + 6))) in
      let last_col = S.trim (S.sub s1 0 ((snd subs) - 1)) in
      (last_col::[], fst_val ^ "," ^ s2)
  | s1, Some s2, false ->
      let res = list_cols_vals s2 del in
      ((s1::fst(res)), snd(res))
  | _, _, _ -> failwith "fail"

(* *)
let rec rid_parenthesis xs =
  List.map
    (fun f -> if S.get f 0 = '(' then S.sub f 1 (S.length f -1)
              else
                if S.get f (S.length f - 1) = ')' then S.sub f 0 (S.length f-1)
                else f) xs
(* used for update. This helper creates a tuple that returns a tuple of all of
the columns and the value that you are replacing, and the rest of the
command for where*)
let rec parse_set (cmd:string) (acc:(string*string) list ) : (string*string) list * string  =
  let (next,commands) = next_word cmd in
  if S.lowercase next = "where" then
    (acc,commands)
  else
    let (set_column,next_commands) = next_word_comma cmd in
    let (column_name,other_string) = next_word set_column in
    let (trivial,other_string) = next_word_quote other_string in
    let column_value = S.sub other_string 0 (S.length other_string -1) in
    let new_acc = (column_name,column_value)::acc in
    parse_set next_commands new_acc

(******** OTHER ********)

let string_to_type str =
  match S.lowercase str with
  | "integer" -> TInt
  | "boolean" -> TBool
  | "float"   -> TFloat
  | "string"  -> TString
  | _         -> failwith "you goofed up big time, brah"

let convert_to_vstring (value:value) : value =
  match value with
  |VInt x -> VString(string_of_int (x))
  |VBool x -> VString(string_of_bool (x))
  |VFloat x -> VString(string_of_float (x))
  |VString x -> value
  |VNull -> VNull

let convert_to_vint (value:value) : value =
  match value with
  |VInt x -> value
  |VBool x -> VNull
  |VFloat x -> VInt(int_of_float x)
  |VString x -> let answer = try VInt(int_of_string x) with
                |Failure z -> VNull
                |Invalid_argument z -> VNull in answer
  |VNull -> VNull

let convert_to_vfloat (value:value) : value =
  match value with
  |VInt x -> VFloat (float_of_int x)
  |VBool x -> VNull
  |VFloat x -> value
  |VString x -> let answer = try VFloat(float_of_string x) with
                |Failure z -> VNull
                |Invalid_argument z -> VNull in answer
  |VNull -> VNull

let convert_to_vbool (value:value) : value =
  match value with
  |VInt x -> VNull
  |VBool x -> value
  |VFloat x -> VNull
  |VString x -> let answer = try VBool(bool_of_string x) with
                |Failure z -> VNull
                |Invalid_argument z -> VNull in answer
  |VNull -> VNull

(* petition to not have alter modify as a command??? please?? *)
let rec convert_col_type col typ =
  if col.typ = typ then col
  else
    match typ with
            |TString -> let new_vals =
              List.map (fun x -> convert_to_vstring x ) col.vals in
              {col with vals = new_vals; typ = TString}
            |TInt -> let new_vals =
              List.map (fun x -> convert_to_vint x ) col.vals in
              {col with vals = new_vals; typ = TInt}
            |TFloat -> let new_vals =
              List.map (fun x -> convert_to_vfloat x) col.vals in
              {col with vals = new_vals; typ = TFloat}
            |TBool ->  let new_vals =
              List.map (fun x -> convert_to_vbool x ) col.vals in
              {col with vals = new_vals; typ = TBool}


let add_nulls tbl =
  if List.length tbl.cols = 0 then []
  else List.map (fun x -> VNull) (List.hd tbl.cols).vals

let add_col tbl cmd =
  let (col_name, cmd_2) = next_word cmd in
  let (col_type, cmd_3) = next_word cmd_2 in
  if cmd_3 = "" then
    if not (List.exists (fun x -> x.name = col_name) tbl.cols) then
      let col_typ = string_to_type col_type in
      let new_col = {name = col_name; vals = add_nulls tbl; typ = col_typ} in
      {tbl with cols = new_col::tbl.cols}
    else failwith "name taken"
  else failwith "not a command"

let drop_col tbl cmd =
  let (col_name, cmd_2) = next_word cmd in
  if cmd_2 = "" then
    let new_tbl = List.filter (fun x -> x.name <> col_name) tbl.cols in
    if List.length tbl.cols = List.length new_tbl then failwith "does not exist"
    else {tbl with cols = new_tbl}
  else failwith "not a command"

(* Takes in the name of a column and returns the associated record *)
let rec find_col_info c_name cols =
  match cols with
  | [] -> failwith "Column doesn't exist"
  | h::t -> if h.name = c_name then h else find_col_info c_name t

(* Takes in list of column names and returns list of column records *)
let rec find_cols c_names cols =
  match c_names with
  | [] -> []
  | h::t -> (find_col_info h cols) :: (find_cols t cols)

(* Replaces the table with [tab_name] in [db] with a different [table] *)
let rec replace_table db tab_name table =
  match db with
  | [] -> []
  | h::t -> if h.title = tab_name then table::t else h::(replace_table t tab_name table)

(* Converts [str] to value type *)
let get_val (str:string) =
  if String.get str 0 = ''' then
    VString (String.sub str 1 ((String.length str)-1))
  else
    try VInt(int_of_string str) with
    | _ -> (try VBool(bool_of_string str) with
            | _ -> (try VFloat(float_of_string str) with
                    | _ -> failwith "Bad value"))

(* Check if a given [value] type-matches a given [val_type]
   (used for checking type of a value before
   adding to a column of [val_type]) *)
let val_ok value val_typ =
  match value, val_typ with
  | VInt _, TInt | VBool _, TBool | VFloat _, TFloat | VString _, TString -> true
  | _, _ -> false

(* Adds a list of strings [vals] to [cols]. [cols_left] keeps track of columns
   that haven't been added to yet.
   Only add a value if its type matches the column's type.
   If [cols] is longer than [vals], add VNull to the rest of the columns *)
let rec add_to_columns vals cols cols_left =
  match vals, cols, cols_left with
  | [], c, [] -> c
  | [], h::t, _ ->
      (* More columns than values given, so append VNull *)
      let new_c = {name=h.name; vals=h.vals @ [VNull]; typ=h.typ} in
      add_to_columns [] (new_c::t) t
  | h::t, _, [] -> failwith "too many values given"
  | h1::t1, h2::t2, _ ->
      (* Convert string to a value type *)
      let h1_val = get_val h1 in
      (* Check if type of values matches column type before appending *)
      if val_ok h1_val h2.typ then
        let new_c = {name=h2.name; vals=h2.vals @ [h1_val]; typ=h2.typ} in
        add_to_columns t1 (new_c::t2) t2
      else
        failwith "mismatched types"
  | _, _, _ -> failwith "bad"

let rec replace_col c cols =
  match cols with
  | [] -> []
  | h::t -> if h.name=c.name then c::t else h::(replace_col c t)

let rec remove_col c cols =
  match cols with
  | [] -> []
  | h::t -> if h.name=c.name then t else h::(remove_col c t)

let rec vnull_to_rest cols all_cols =
  match cols with
  | [] -> all_cols
  | h::t ->
      let new_c = {name=h.name; vals=h.vals @ [VNull]; typ=h.typ} in
      vnull_to_rest t (replace_col new_c all_cols)


let rec add_to_some_cols vals cols cols_left all_cols =
  if vals <> [] && cols_left = [] then failwith "too many values given"
  else
    match vals, cols with
    | [], _ -> vnull_to_rest cols_left all_cols
    | h::t, h2::t2 ->
        let c = find_col_info h2 all_cols in
        (* Convert string to a value type *)
        let h_val = get_val h in
        (* Check if type of values matches column type before appending *)
        if val_ok h_val c.typ then
          let new_c = {name=c.name; vals=c.vals @ [h_val]; typ=c.typ} in
          add_to_some_cols t t2 (remove_col new_c cols_left) (replace_col new_c all_cols)
        else
          failwith "mismatched_types"
    | _, _ -> failwith "fail"

(* deprecated? *)
let modify_col tbl cmd =
  let (col_name, cmd_2) = next_word cmd in
  let (col_type, cmd_3) = next_word cmd_2 in
  if cmd_3 = "" then
    let old_col = try List.find (fun x -> x.name = col_name) tbl.cols with
                  | _ -> failwith "can't find it brah" in
    let col_typ = string_to_type col_type in
    let new_cols = List.map (fun x -> if x.name = col_name
                          then convert_col_type old_col col_typ
                       else
                          x) tbl.cols  in
    {tbl with cols = new_cols}

  else failwith "not a command"

(* old code *
let rec delete_column (column_list: column list) (column_name: string) : column list=
  match column_list with
  | [] -> []
  | h::t -> if h.name = column_name
              then delete_column t column_name
            else
              h::delete_column(t) (column_name)

let rec modify_column (column_list: column list) (column_name: string)
  (original_name:string): column list =
  match column_list with
  | [] -> []
  | h::t -> if h.name = original_name then
              {h with name = column_name}::t
           else
              h::modify_column (t) (column_name) (original_name) *)

(* Finds table with [tbl_name] in [db] *)
let find_table db tbl_name =
  try List.find (fun x -> x.title = tbl_name) db with
  | _ -> failwith "table does not exist"

let where t r = [1;2;3]

(*finds column objects from the table and a column NAME list*)
let find_cols (tbl:table) (col_names:string list) : column list =
  let rec helper clist colnames acc =
    (match clist with
    |[] -> acc
    |h::t -> if List.mem h.name colnames then let newacc = acc @ [h] in
            helper t colnames newacc
            else helper t colnames acc) in
  helper (tbl.cols) col_names []

(*returns a new value list from a col's value list and an index list*)
let rec new_values (vals : value list) (ind : int list) (acc : value list) =
  match ind with
  |[] -> acc
  |h::t -> let newacc = acc @ [List.nth vals h] in new_values vals t newacc

(*builds new columns out of new values specified by the int list*)
let rec new_cols (cl : column list) (i : int list) (acc : column list) =
  match cl with
  |[] -> acc
  |h::t -> let newacc = acc @ [{name = h.name; vals = new_values (h.vals)(i)([]);
            typ = h.typ}] in
            new_cols t i newacc


(* This helper function takes in a column's value list, the list of ints from
  where that must be replaced, and the value it is replacing it with to create
  a new value list*)
let rec update_values (vals : value list) (ind : int list) (value:string)
  (counter: int) (val_type: val_type): value list =
  match ind,vals with
  |[],_ -> vals
  |h::t,h'::t' -> if h = counter then
                    let new_value = (match val_type with
                      |TInt -> convert_to_vint (VString(value))
                      |TBool -> convert_to_vbool (VString(value))
                      |TFloat -> convert_to_vfloat (VString(value))
                      |TString -> convert_to_vstring (VString(value))) in
                    new_value::(update_values t' t value (counter+1) val_type)
                  else
                    h'::(update_values t' ind value (counter+1) val_type)
  |_,_ -> failwith "Something happened that shouldn't have happened"

(* This helper function takes in a table's column list, the list of ints from
  where, an accumulator of a new column list, and the value_list that was
  parsed from the SET, and returns a new column list*)
let rec create_cols (cl : column list) (i : int list) (acc : column list)
  (val_list: (string*string) list) : column list=
  let rec find_col (column: column) (val_list:(string*string) list) (i:int list) =
    match val_list with
    |[] -> column
    |h::t -> if column.name = (fst(h)) then
                let new_values = update_values (column.vals) i (snd(h)) 0 column.typ in
              {name = column.name; vals = new_values; typ = column.typ}
              else
                find_col column t i in
  match cl with
  |[] -> acc
  |h::t -> let col = find_col h val_list i in
            create_cols t i (col::acc) val_list




let print x = print_string "hi"

let match_string x y = Some 1

let index_filter x y = []

(**********************)
(*      Commands      *)
(**********************)

(*returns a db restricted to the requirements given*)
let select (db:db) (cmd : string) : db =
  let findex' = match_string " from " (S.lowercase cmd) in
  let findex = (match findex' with
              |None -> failwith "missing \"from\""
              |Some s -> s) in
  let tname = fst(next_word(S.sub cmd (findex+6) ((S.length cmd)-(findex+6)))) in
  let table = find_table db tname in
  let star = match_string " * " cmd in
  let whre = match_string " where " (S.lowercase cmd) in
  (*findex is the index where "from" begins. tname is the table name, and table
  * is the table object. star is the index of the * character and whre is the index
  * of "where". whre and star can be none, meaning we should handle the query
  * differently.*)
  match whre with
  (*If there is no "where" and no star, use find_cols to find the columns asked
  * for. If there is a star, however, print all the columns in the table.*)
  |None -> (match star with
          |None -> let colnames = list_chunks (S.sub cmd 0 findex) ',' in
                  let collist = find_cols table colnames in
                  let _ = print collist in
                  db
          |Some v -> let _ = print table.cols in
                    db)
  (*If there is a "where" and there is a star, use where() to find the index list
  *and use new_cols to get the new columns built out of values at the index list.
  *If there is no star, restrict the initial columns to be the ones specified
  *before using where() and new_cols.*)
  |Some i -> let collist = (match star with
                          |None ->let colnames =
                                  list_chunks (S.sub cmd 0 findex) ',' in
                                  find_cols table colnames
                          |Some i -> table.cols) in
              let indecies =
              where table (snd(next_word(S.sub cmd i ((S.length cmd)-i)))) in
              let restrcols = index_filter indecies collist in
              let newcols = new_cols restrcols indecies [] in
              let _ = print newcols in
              db



(******** CREATE ********)


(*creates a new table with the names that are given*)
let create (db:db) (cmd:string) : db =
  let (command,next_commands) = next_word cmd in (* grabs the word TABLE, but it is worthless *)
  let (table_name,next_commands) = next_word next_commands in (* grabs the table name *)
  let list_columns = list_chunks next_commands ',' in (* Since the rest of the commands are delimited by commas, list chunks will divide it into a list*)
  let new_table = List.fold_left (fun a c -> add_col a c)
    {title = table_name ; cols = []} list_columns in
  new_table::db
  (* the list.fold call will call add_col on the accumulator, which is an empty
  table, and it will add the columns into the new table *)

(* old code *
  let (tbl_name, cmd_2) = next_word cmd in
  {title = tbl_name; cols = []}::db *)


(******** INSERT ********)

(* inserts a row into a given table with its
 * categories and corresponding values *)
let insert (db:db) (req:string) : db =
  (* Find table name *)
  let tab_name, rest = next_word req in
  let snd_word, rest' = next_word rest in
  let snd_word_lower = String.lowercase snd_word in
  let table = find_table db tab_name in
  (* If second word lowercase is "values," then user didn't pass in specific
     column names *)
  if snd_word_lower = "values" then
    (* Get rid of beginning/ending parenthesis around input values *)
    let values = String.sub rest' 0 ((String.length rest') - 1) in
    (* Convert to value list and add to columns *)
    let val_lst = list_chunks values ',' in
    let columns = add_to_columns val_lst table.cols table.cols in
    (* Construct new list of columns for this table and replace table *)
    let new_table = {title=table.title; cols=columns} in
    replace_table db tab_name new_table
  else
    let cols_vals = list_cols_vals (rest') ',' in
    let cols = rid_parenthesis (fst cols_vals) in
    let vals = rid_parenthesis (list_chunks (snd cols_vals) ',') in
    let new_cols = add_to_some_cols vals cols table.cols table.cols in
    let new_table = {title=table.title; cols=new_cols} in
    replace_table db tab_name new_table

(******** UPDATE ********)

(* updates a record in the table with new values matched with
 * their corresponding categories.
 * [new_vals] is in the form [("cat_name", "new_val_at_the_row")]
 * i.e.
 * UPDATE Customers
 * SET ContactName = "Alfred", City = "Hamburg"
 * WHERE CustomerName = "Alfred 2.0" *)
let update (db:db) (cmd:string) : db =
  let (tab_name,next_commands) = next_word cmd in
  let table = find_table db tab_name in
  let (keyword_set,next_commands) = next_word next_commands in
    if S.lowercase keyword_set = "set" then
      let (val_list,next_commands) = parse_set next_commands [] in
      let indices = where table next_commands in
      let new_cols = create_cols table.cols indices [] val_list in
      let new_table = {title = tab_name; cols = new_cols} in
      replace_table db tab_name new_table
    else
      failwith "Did not provide SET parameters"

(******** DELETE ********)

(* deletes a given row from a given table *)
(* WARNING: not tail recursive do to use of List.map *)
let delete (db:db) (cmd:string) : db =
  let (tbl_name, cmd_2) = next_word cmd in
  let tbl = find_table db tbl_name in
  let match_lst = extra_word "where" (where tbl) cmd_2 in
  let remove_matches tbl =
    let icols = List.mapi (fun i x -> (i,x)) tbl.cols in
    let new_cols = List.filter (fun x -> List.mem (fst x) match_lst) icols in
    {tbl with cols = List.map snd new_cols} in
  List.map (fun x -> if x.title = tbl_name then remove_matches x else x) db

(* old code *
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
  replace db newtable [] *)


(******** DROP ********)

(*deletes a given table*)
let rec drop (db:db) (cmd:string) : db =
  let (tbl_name, cmd_2) = next_word cmd in
  if cmd_2 = "" then
    match db with
    | [] -> []
    | h::t -> if h.title = tbl_name then t else h::(drop t tbl_name)
  else failwith "not a valid command"


(******** ALTER ********)

(*adds, removes, or modifies a given category from a given table*)
let alter (db:db) (cmd:string) : db =
  let (tbl_name, cmd_2) = next_word cmd in
  let tbl = find_table db tbl_name in
  let (cmd_typ, cmd_3) = next_word cmd_2 in
  let new_tbl = match S.lowercase cmd_typ with
                | "add"    -> add_col tbl cmd_3
                | "drop"   -> extra_word "column" (drop_col tbl) cmd_3
                | "modify" -> extra_word "column" (modify_col tbl) cmd_3
                | _        -> failwith "not a command" in
  List.map (fun x -> if x.title = new_tbl.title then new_tbl else x) db

(* old code *
  let (col_name, cmd_4) = next_word cmd_3 in
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
  do_command db table_name column_name alter_command modify_column_name *)


(* evaluates the commands given to it and returns an updated db *)
let eval (db:db) (commands:string) : db =
  let (cmd, tail) = next_word commands in
  match S.lowercase cmd with
  | "select" -> select db tail
  | "create" -> extra_word "table" (create db) tail
  | "insert" -> (* extra_word "into" (insert db) tail *) failwith "TODO"
  | "update" -> (* update db tail *) failwith "TODO"
  | "delete" -> delete db tail
  | "drop"   -> extra_word "table" (drop db) tail
  | "alter"  -> extra_word "table" (alter db) tail
  | _        -> failwith "not a command"

(* takes an existing db and user input to evaluate commands and return an
 * updated db *)
let revise_db (db:db) (input:string) : db =
  eval db input
