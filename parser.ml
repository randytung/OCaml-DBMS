open Yojson.Basic.Util
open Types

(* Parses the portion of the JSON representing a single value *)
let create_val (typ:val_type) (v:Yojson.Basic.json) : value =
  let str = [v] |> filter_string |> List.hd in
  string_to_val str typ

(* Parses the portion of the JSON representing a single column *)
let create_col (col:Yojson.Basic.json) : column =
  let name' = [col] |> filter_member "name" |> filter_string |> List.hd in
  let typ' = [col] |> filter_member "typ" |> filter_string |> List.hd in
  let typ'' = string_to_type typ' in
  let val_lst = [col] |> filter_member "values" |> flatten in
  {name = name';
   vals = List.rev (List.rev_map (create_val typ'') val_lst);
   typ = typ''}

(* Parses the portion of the JSON representing a single table *)
let create_tbl (tbl:Yojson.Basic.json) : table =
  let title' = [tbl] |> filter_member "title" |> filter_string |> List.hd in
  let col_lst = [tbl] |> filter_member "columns" |> flatten in
  {title = title'; cols = List.map create_col col_lst}

let create_db (js:Yojson.Basic.json) : db =
  let tbl_lst = [js] |> filter_member "tables" |> flatten in
  List.map create_tbl tbl_lst