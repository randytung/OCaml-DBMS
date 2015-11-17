open Yojson.Basic.Util
open Types

exception Parse_Exception

(* Parses the portion of the JSON representing a single value *)
let create_val (typ:val_type) (v:Yojson.Basic.json) : value =
  let str = [v] |> filter_string |> List.hd in
  match typ with
  | TInt -> VInt (try (int_of_string str) with _ -> raise Parse_Exception)
  | TFloat -> VFloat (try (float_of_string str) with _ -> raise Parse_Exception)
  | TString -> VString str

(* Parses the portion of the JSON representing a single category *)
let create_cat (cat:Yojson.Basic.json) : category =
  let id = [cat] |> filter_member "id" |> filter_string |> List.hd in
  let typ = [cat] |> filter_member "typeof" |> filter_string |> List.hd in
  let typ' = match typ with
             | "integer" -> TInt
             | "float" -> TFloat
             | _ -> TString in
  let val_lst = [cat] |> filter_member "vals" |> flatten in
  {name = id; vals = List.map (create_val typ') val_lst; typeof = typ'}

(* Parses the portion of the JSON representing a single table *)
let create_tbl (tbl:Yojson.Basic.json) : table =
  let id = [tbl] |> filter_member "id" |> filter_string |> List.hd in
  let cat_lst = [tbl] |> filter_member "categories" |> flatten in
  {name = id; cats = List.map create_cat cat_lst}

let create_db (js:Yojson.Basic.json) : db =
  let tbl_lst = [js] |> filter_member "tables" |> flatten in
  List.map create_tbl tbl_lst