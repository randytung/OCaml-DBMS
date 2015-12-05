open Types
open Basic_parse

(**********************)
(*  Helper Functions  *)
(**********************)

(**** Table Manipulation ****)

(* returns table with [tbl_name] from [db] if it exists *)
let find_table (db:db) (tbl_name:string) : table =
  try List.find (fun x -> x.title = tbl_name) db with
  | _ -> failwith "table does not exist"

(* replaces a table in [db] with [new_tbl] if they share the same title *)
let replace_table (db:db) (new_tbl:table) : db =
  List.map (fun x -> if x.title = new_tbl.title then new_tbl else x) db

(**** Column Manipulation ****)

(* returns only the columns in [tbl] whose names are in [col_names] *)
let find_cols (tbl:table) (col_names:string list) : column list =
  let f x = List.find (fun y -> y.name = x) tbl.cols in
  try List.rev (List.rev_map f col_names) with _ -> failwith "not a valid name"

(* changes [col] and its values to [typ] if possible without major data loss *)
let convert_col_type (col:column) (typ:val_type) : column =
  if col.typ = typ then col
  else
    let f =
      match typ, col.typ with
      | TString, _ -> val_to_vstring
      | TInt, TFloat | TInt, TString -> val_to_vint
      | TFloat, TInt | TFloat, TString -> val_to_vfloat
      | TBool, TString -> val_to_vbool
      | _, _ -> failwith "can't convert col type to new type" in
    let new_vals = List.rev_map f col.vals in
    {col with vals = (List.rev new_vals); typ = typ}

(* adds a col to [tbl] as specified in [cmd] *)
let add_col (tbl:table) (cmd:string) : table =
  let (col_name, col_type) = next_word cmd in
  if List.exists (fun x -> x.name = col_name) tbl.cols
     || not (check_format col_name) then failwith "invalid name"
  else
    let col_typ = string_to_type col_type in
    let add_nulls tbl =
      if List.length tbl.cols = 0 then []
      else List.rev_map (fun x -> VNull) (List.hd tbl.cols).vals in
    let new_col = {name = col_name; vals = add_nulls tbl; typ = col_typ} in
    {tbl with cols = List.rev (new_col::(List.rev tbl.cols))}

(* removes a col from [tbl] as specified in [cmd] *)
let drop_col (tbl:table) (cmd:string) : table =
  let (col_name, cmd_2) = next_word cmd in
  if cmd_2 <> "" then failwith "not a command"
  else
    let new_tbl = List.filter (fun x -> x.name <> col_name) tbl.cols in
    if List.length tbl.cols = List.length new_tbl
    then failwith "does not exist"
    else {tbl with cols = new_tbl}

(* converts a col in [tbl] (and its values) to
 * a new value type as specified through [cmd] *)
let modify_col (tbl:table) (cmd:string) : table =
  let (col_name, col_type) = next_word cmd in
  let old_col = try List.find (fun x -> x.name = col_name) tbl.cols
                with _ -> failwith "invalid column name" in
  let col_typ = string_to_type col_type in
  let try_modify col =
    if col.name = col_name
    then convert_col_type old_col col_typ
    else col in
  let new_cols = List.map try_modify tbl.cols  in
  {tbl with cols = new_cols}

(* modifies columns in [cl] to only have the values whose indices are in [il] *)
let filter_cols (cl:column list) (il:int list) (eq:bool) : column list =
  (* filters only the elements from [element_lst]
   * whose indices are in [index_lst] *)
  let filter_vals element_lst index_lst =
    let rec n_v il el i acc =
      match il, el with
      | [], eh::et -> if eq then acc else n_v il et (i+1) (eh::acc)
      | [], [] -> acc
      | _, [] -> if eq then failwith "more indices than elements" else acc
      | ih::it, eh::et -> let (eq_acc, neq_acc) =
                            if eq then (eh::acc,acc) else (acc,eh::acc) in
                          if ih=i then n_v it et (i+1) eq_acc
                          else n_v il et (i+1) neq_acc in
    List.rev (n_v (List.sort_uniq (-) index_lst) element_lst 0 []) in
  List.rev (List.rev_map (fun x -> {x with vals = filter_vals (x.vals) il}) cl)

(**** Misc ****)

(* prints the given [col_lst] as a table with name [tbl_name] *)
let print (tbl_name:string) (col_lst:column list) : unit =
  Printer.print tbl_name col_lst

let where_dels = [" "; ","; "("; ")"; "="; ">"; "<"; "or"; "and";
                  "between"; "like"; "in"; "isnull"; "notnull"]

(* returns indices of rows in [tbl] that satisfy parameters in [cmd] *)
let where (tbl:table) (cmd:string) : int list =
  let tokens = Lex.lex cmd where_dels in
  Where_parse.parse (tbl.cols) tokens

let set_dels = [" "; ","; "("; ")"; "="; "+"; "-";"*"; "/"; "and"; "or"]

(* returns association list between a column name and new values of each column
 * in the set of columns from [tbl] which satisfy parameters in [cmd] *)
let set (tbl:table) (cmd:string) : (string * value list) list =
  let tokens = Lex.lex cmd set_dels in
  Set_parse.parse (tbl.cols) tokens


(**********************)
(*      Commands      *)
(**********************)

(******** SELECT ********)

(* returns a db restricted to the requirements given *)
let select (db:db) (cmd : string) : db =
(* from_split splits cmd into two strings:
 * one from before the word "from" and one after it *)
  let from_split = str_split cmd " from " in
  let (before, after) = match from_split with
                        | None -> failwith "missing 'from'"
                        | Some (bef, aft) -> (S.trim bef, S.trim aft) in
  let (tbl_name, cmd_2) = next_word after in
  let tbl = find_table db tbl_name in
(* If the string before "from" was a star, then all cols are selected.
 * Otherwise, only the cols listed are selectd. *)
  let col_lst = match before with
                | "*" -> tbl.cols
                | _ -> find_cols tbl (chr_split_lst before ',' true) in
(* If "where" is present, match values according to where.
 * Otherwise, match all values. *)
  let prnt_cols = (try (let matches = extra_word "where" (where tbl) cmd_2 in
                       filter_cols col_lst matches true)
                   with _ -> col_lst) in
  (print tbl_name prnt_cols; db)


(******** CREATE ********)

(* creates a new table with the names that are given *)
let create (db:db) (cmd:string) : db =
  let (tbl_name, cmd_2) = next_word cmd in
  if List.exists (fun x -> x.title = tbl_name) db || not (check_format tbl_name)
  then failwith "not a valid title name"
  else
    let tbl_shell = {title = tbl_name; cols = []} in
    let col_name_str = trim_parens (S.trim cmd_2) in
    if S.trim col_name_str = "" then tbl_shell::db else
    let col_names_typs = chr_split_lst col_name_str ',' true in
    let new_tbl = List.fold_left add_col tbl_shell col_names_typs in
    new_tbl::db
  (* the list.fold call will call add_col on the accumulator, which is an empty
  table, and it will add the columns into the new table *)


(******** INSERT ********)

(* inserts a row into a given table with its
 * categories and corresponding values *)
let insert (db:db) (cmd:string) : db =
  let (tbl_name, cmd_2) = next_word cmd in
  let tbl = find_table db tbl_name in
  let values_split = str_split cmd_2 "values" in
  let (before, after) =
    match values_split with
    | None -> failwith "no keyword 'values'"
    | Some (bef, aft) -> (S.trim bef, S.trim aft) in
  let col_lst =
    if before = "" then tbl.cols
    else
      let col_names = trim_parens before in
      find_cols tbl (chr_split_lst col_names ',' true) in
  let val_strs =
      let val_str = trim_parens after in
      chr_split_lst val_str ',' true in
  let cols_vals = try List.combine col_lst val_strs
                  with _ -> failwith "unmatched cols and vals" in
  let add_val cols_vals col =
    let new_val = try Some (List.assoc col cols_vals) with _ -> None in
    match new_val with
    | None -> {col with vals = col.vals @ [VNull]}
    | Some v -> {col with vals = col.vals @ [string_to_val v col.typ]} in
  let new_cols = List.map (add_val cols_vals) tbl.cols in
  let new_tbl = {tbl with cols = new_cols} in
  replace_table db new_tbl


(******** UPDATE ********)

(* updates a record in the table with new values matched with
 * their corresponding categories. *)
let update (db:db) (cmd:string) : db =
  let (tbl_name, cmd_2) = next_word cmd in
  let tbl = find_table db tbl_name in
  let where_split = extra_word "set" (fun x -> str_split x "where") cmd_2 in
  let (before, after) =
    match where_split with
    | None -> failwith "missing keyword 'where'"
    | Some (bef, aft) -> (S.trim bef, S.trim aft) in
  let new_cols =
    if before = "" then failwith "missing set parameters"
    else set tbl before in
  let indices =
    if after = "" then failwith "missing where parameters"
    else where tbl after in
  let update_vals col_vals new_vals indices =
    let rec step_func cvs nvs is i acc =
      match cvs, nvs with
      | col_h::col_t, new_h::new_t ->
        if List.mem i is then step_func col_t new_t is (i+1) (new_h::acc)
        else step_func col_t new_t is (i+1) (col_h::acc)
      | _, _ -> acc in
    List.rev (step_func col_vals new_vals indices 0 []) in
  let update_filter col =
    let new_vals = try Some (List.assoc col.name new_cols) with _ -> None in
    match new_vals with
    | Some vs -> {col with vals = update_vals col.vals vs indices}
    | None -> col in
  let new_cols = List.map update_filter tbl.cols in
  let new_tbl = {tbl with cols = new_cols} in
  replace_table db new_tbl


(******** DELETE ********)

(* deletes rows from a given table *)
let delete (db:db) (cmd:string) : db =
  let (tbl_name, cmd_2) = next_word cmd in
  let tbl = find_table db tbl_name in
  let matches = extra_word "where" (where tbl) cmd_2 in
  let new_tbl = {tbl with cols = filter_cols tbl.cols matches false} in
  replace_table db new_tbl


(******** DROP ********)

(* deletes a given table *)
let drop (db:db) (cmd:string) : db =
  let (tbl_name, cmd_2) = next_word cmd in
  let _ = find_table db tbl_name in
  if cmd_2 <> "" then failwith "not a valid command"
  else List.filter (fun x -> x.title <> tbl_name) db


(******** ALTER ********)

(* adds, removes, or modifies a given category from a given table *)
let alter (db:db) (cmd:string) : db =
  let (tbl_name, cmd_2) = next_word cmd in
  let tbl = find_table db tbl_name in
  let (cmd_typ, cmd_3) = next_word cmd_2 in
  let new_tbl = match S.lowercase cmd_typ with
                | "add"    -> add_col tbl cmd_3
                | "drop"   -> extra_word "column" (drop_col tbl) cmd_3
                | "modify" -> extra_word "column" (modify_col tbl) cmd_3
                | _        -> failwith "not a command" in
  replace_table db new_tbl


let eval (db:db) (cmd:string) : db =
  let (op, cmd_2) = next_word cmd in
  match S.lowercase op with
  | "select" -> select db cmd_2
  | "create" -> extra_word "table" (create db) cmd_2
  | "insert" -> extra_word "into" (insert db) cmd_2
  | "update" -> update db cmd_2
  | "delete" -> extra_word "from" (delete db) cmd_2
  | "drop"   -> extra_word "table" (drop db) cmd_2
  | "alter"  -> extra_word "table" (alter db) cmd_2
  | _        -> failwith "not a command"