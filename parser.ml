open Yojson
open Types

exception Parse_Exception

(*creates a list of all table names from the JSON*)
let parse_table_names (js:Yojson.Basic.Json) : string list =
  failwith "unimplemented"

(*creates a list of all category names belonging to a table*)
let parse_cat_names (js:Yojson.Basic.Json) (tab_name:string) : string list =
  failwith "unimplemented"

(*parses a given category's type*)
let parse_cat_type (js:Yojson.Basic.Json) (cat_name:string) : string =
  failwith "unimplemented"

(*parses and creates a list of values from a category based on its inferred type*)
let create_values (js:Yojson.Basic.Json) (cat_name:string) (typ:string) : value list =
  failwith "unimplemented"

(*creates the list of categories that belong in the table from the table's name,
* the category names, category types, and lists of values for each category*)
let create_categories (tab_name:string) (cat_names:string list) (cat_typs:string list)
                      (val_lsts:value list list) : category list =
  failwith "unimplemented"

(*creates a table from its name and its categories*)
let create_table (tab_name:string) (cats:category list) : table =
  failwith "unimplemented"

(*creates the db from the list of tables*)
let db_from_tables (tabs:table list) : db =
  failwith "unimplemented"

(*main function which calls all others, creating a db from a given json*)
let create_db (js:Yojson.Basic.Json) : db =
  failwith "unimplemented"
