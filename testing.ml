open Types
open Operations

(* Tests for accuracy *)
TEST "Select" = failwith ""
TEST "Create" = failwith ""
TEST "Insert" = failwith ""
TEST "Update" = failwith ""
TEST "Delete" = failwith ""
TEST "Drop" = failwith ""




let col1 = {name = "Name"; vals = [VString "Grant Stento"; VString "Kristy Liao"];
       typ = TString}
let col2 = {name = "Phone Number"; vals = [VInt 456; VInt 654]; typ = TInt}

let db = [{title = "Phone_book"; cols = [col1;col2]}]
TEST "Alter_Add" = let new_db = alter db "Phone_book ADD Address string" in
  let col3 = {name = "Address"; vals = [VNull; VNull]; typ = TString} in
  let add_db = [{title = "Phone_book"; cols = [col3;col1;col2]}] in
  new_db = add_db


(* Tests for exceptions *)
TEST_UNIT "Select" = failwith ""