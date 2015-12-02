open Yojson.Basic.Util
open Types

let build_value v =
  "\"" ^ (val_to_string v) ^ "\", "

let build_column col =
  let val_str = List.fold_left (fun x y -> x ^ (build_value y)) "" (col.vals) in
  "        {\n          \"name\": \"" ^ col.name ^
  "\",\n          \"values\": [" ^
  (String.sub val_str 0 (max 0 ((String.length val_str) - 2))) ^
  "],\n          \"typ\": \"" ^ (type_to_string col.typ) ^
  "\"\n        },\n"

let build_table tbl =
  let col_str = List.fold_left (fun x y -> x ^ (build_column y)) "" (tbl.cols) in
  "    {\n      \"title\": \"" ^ tbl.title ^
  "\",\n      \"columns\": [\n" ^
  (String.sub col_str 0 (max 0 ((String.length col_str) - 2))) ^
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