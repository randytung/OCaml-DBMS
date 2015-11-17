open Yojson.Basic.Util
open Types

let convert_value = function
  | VInt x -> string_of_int x
  | VFloat x -> string_of_float x
  | VString x -> x

let convert_type = function
  | TInt -> "integer"
  | TFloat -> "float"
  | TString -> "string"

let build_value v =
  "\"" ^ (convert_value v) ^ "\", "

let build_category cat =
  let val_str = List.fold_left (fun x y -> x ^ (build_value y)) "" (cat.vals) in
  "        {\n          \"name\": \"" ^ cat.name ^
  "\",\n          \"values\": [" ^
  (String.sub val_str 0 (max 0 ((String.length val_str) - 2))) ^
  "],\n          \"typ\": \"" ^ (convert_type cat.typ) ^
  "\"\n        },\n"

let build_table tbl =
  let cat_str = List.fold_left (fun x y -> x ^ (build_category y)) "" (tbl.cats) in
  "    {\n      \"title\": \"" ^ tbl.title ^
  "\",\n      \"categories\": [\n" ^
  (String.sub cat_str 0 (max 0 ((String.length cat_str) - 2))) ^
  "\n      ]\n    },\n"

(*builds a JSON file from the given db*)
let build (db:db) : string =
  let tbl_str = List.fold_left (fun x y -> x ^ (build_table y)) "" db in
  "{\n  \"tables\": [\n" ^
  (String.sub tbl_str 0 (max 0 ((String.length tbl_str) - 2))) ^ "\n  ]\n}"

(*saves the JSON file to a given file location, overwrites old JSON*)
let save (db:db) (file_name:string) : unit =
  let overwrite = open_out file_name in
  Printf.fprintf overwrite "%s" (build db);
  close_out overwrite
(* reference: https://ocaml.org/learn/tutorials/file_manipulation.html *)