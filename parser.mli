open Yojson
open Types

exception Parse_Exception

(*creates a list of all table names from the JSON*)
val parse_table_names : Yojson.Basic.Json -> string list

(*creates a list of all category names belonging to a table*)
val parse_cat_names : Yojson.Basic.Json -> string -> string list

(*parses a given category's type*)
val parse_cat_type : Yojson.Basic.Json -> string -> string

(*parses and creates a list of values from a category based on its inferred type*)
val create_values : Yojson.Basic.Json -> string -> string -> value list

(*creates the list of categories that belong in the table from the table's name,
* the category names, category types, and lists of values for each category*)
val create_categories : string -> string list -> string list -> value list list
                      -> category list

(*creates a table from its name and its categories*)
val create_table : string -> string -> category list -> table

(*creates the db from the list of tables*)
val db_from_tables : table list -> db

(*main function which calls all others, creating a db from a given json*)
val create_db : Yojson.Basic.Json -> db
