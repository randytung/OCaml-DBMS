open Yojson.Basic.Util
open Types

exception Parse_Exception

(* Parses the portion of the JSON representing a single value *)
let create_val (typ:val_type) (v:Yojson.Basic.json) : value =
  let str = [v] |> filter_string |> List.hd in
  let parse_fail () = raise Parse_Exception in
  if str = "%NaN%" then VNull
  else match typ with
       | TInt -> (try VInt (int_of_string str) with _ -> parse_fail ())
       | TBool -> (try VBool (bool_of_string str) with _ -> parse_fail ())
       | TFloat -> (try VFloat (float_of_string str) with _ -> parse_fail ())
       | TString -> VString str

(* Parses the portion of the JSON representing a single column *)
let create_col (col:Yojson.Basic.json) : column =
  let name' = [col] |> filter_member "name" |> filter_string |> List.hd in
  let typ' = [col] |> filter_member "typ" |> filter_string |> List.hd in
  let typ'' = match typ' with
             | "integer" -> TInt
             | "boolean" -> TBool
             | "float" -> TFloat
             | _ -> TString in
  let val_lst = [col] |> filter_member "values" |> flatten in
  {name = name'; vals = List.map (create_val typ'') val_lst; typ = typ''}

(* Parses the portion of the JSON representing a single table *)
let create_tbl (tbl:Yojson.Basic.json) : table =
  let title' = [tbl] |> filter_member "title" |> filter_string |> List.hd in
  let col_lst = [tbl] |> filter_member "columns" |> flatten in
  {title = title'; cols = List.map create_col col_lst}

let create_db (js:Yojson.Basic.json) : db =
  let tbl_lst = [js] |> filter_member "tables" |> flatten in
  List.map create_tbl tbl_lst

 let json2 = Yojson.Basic.from_file "phonebook.json"

