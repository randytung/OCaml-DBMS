open Yojson.Basic.Util
open Types

(* Error raised when JSON can't be properly parsed *)
exception Parse_Exception

(** Parses a JSON following the provided schema.json and turns it into a
 *  db data structure. Fails if the JSON provided can't be properly parsed.*)
val create_db : Yojson.Basic.json -> db