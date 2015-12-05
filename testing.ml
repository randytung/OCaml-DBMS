open Types
open Operations
open Assertions

(* Tests for accuracy *)

let names = {name="Name"; vals=[VString "Arthur"; VString "Grant"]; typ=TString}
let nums = {name="Number"; vals=[VInt 6; VInt 5]; typ=TInt}

let db = [{title="Phonebook"; cols=[names; nums]}]

TEST "Select" = (eval (db) "Select Name,Number FROM Phonebook") = db

TEST "Create" =
  (*test with correct input*)
  let db_tbl = eval  db "create table Student (Name string,netid string)" in
  let name =  {name="Name"; vals=[]; typ=TString} in
  let netid = {name="netid"; vals=[]; typ=TString} in
  let check_db = [{title="Student"; cols = [name;netid]};{title="Phonebook";
    cols=[names; nums]} ] in
  let test1 = check_db = db_tbl in
  (*test with empty columns*)
  let db_tbl2 = eval db "create table People ()" in
    let check_db2 = [{title="People"; cols = []};{title="Phonebook";
    cols = [names; nums]}] in
  let test2 = check_db2 = db_tbl2 in
  (*testing with existing table name*)
  let test3 = try let _ = eval db "create table Phonebook ()" in false
              with _ -> true in
  (*testing with nonexisting type*)
  let test4 = try let _  = eval db "create table Person (Name int, Number cheese)" in false
              with _ -> true in

  test2 && test3 && test4 && test1

let add_val col v =
  let new_vals = col.vals @ [v] in
  {name=col.name; vals=new_vals; typ=col.typ}

TEST "Insert normally" =
  let db' = eval db "insert into Phonebook VALUES ('Randy', 7)" in
  let names' = add_val names (VString "Randy") in
  let nums' = add_val nums (VInt 7) in
  db' = [{title="Phonebook"; cols=[names'; nums']}]

TEST "Insert with spaces" =
  let db' = eval db "insert into Phonebook VALUES ('Kristy  ', 2)" in
  let names' = add_val names (VString "Kristy  ") in
  let nums' = add_val nums (VInt 2) in
  db' = [{title="Phonebook"; cols=[names'; nums']}]

TEST "Insert 1 value" =
  let db' = eval db "insert into Phonebook (Name) VALUES ('Ellen')" in
  let names' = add_val names (VString "Ellen") in
  let nums' = add_val nums (VNull) in
  db' = [{title="Phonebook"; cols=[names'; nums']}]

TEST "Insert with 2 specified columns" =
  let db' = eval db "insert into Phonebook (Number, Name) VALUES (9, 'Donald')" in
  let names' = add_val names (VString "Donald") in
  let nums' = add_val nums (VInt 9) in
  db' = [{title="Phonebook"; cols=[names'; nums']}]

let stuff1 = {name="Stuff 1"; vals=[VNull; VNull]; typ=TString}
let stuff2 = {name="Stuff 2"; vals=[VNull; VNull]; typ=TInt}
let stuff3 = {name="Stuff 3"; vals=[VNull; VNull]; typ=TBool}
let stuff4 = {name="Stuff 4"; vals=[VNull; VNull]; typ=TBool}
let stuff5 = {name="Stuff 5"; vals=[VNull; VNull]; typ=TFloat}

let big_db = [{title="Stuffs"; cols=[stuff1; stuff2; stuff3; stuff4; stuff5]}]

TEST "Insert with multiple specified columns" =
  let db' = eval big_db ("insert into Stuffs (Stuff 2, Stuff 5, Stuff 4, Stuff 3) " ^
            "VALUES (1, 3.4, true, false)") in
  let stuff1' = add_val stuff1 VNull in
  let stuff3' = add_val stuff3 (VBool false) in
  let stuff2' = add_val stuff2 (VInt 1) in
  let stuff5' = add_val stuff5 (VFloat 3.4) in
  let stuff4' = add_val stuff4 (VBool true) in
  db' = [{title="Stuffs"; cols=[stuff1'; stuff2'; stuff3'; stuff4'; stuff5']}]


TEST "Insert certain columns without specifying column names" =
  try let _  = eval db "insert into Phonebook VALUES ('Ellen')" in false
  with _ -> true

TEST "Insert without values" =
  try let _  = eval db "insert into Phonebook ('Ellen')" in false
  with _ -> true

TEST "Insert uneven number of columns and values" =
  try let _  = eval db "insert into Phonebook (Name, Number) VALUES ('Ellen')" in false
  with _ -> true

let names = {name="Name"; vals=[VString "Arthur"; VString "Grant"]; typ=TString}
let nums = {name="Number"; vals=[VInt 6; VInt 5]; typ=TInt}

let db_update = [{title="Phonebook"; cols=[names; nums]}]


TEST "Update" =
  (*regular input of update*)
  let test1 = eval db_update "update Phonebook SET Name = 'Wang' WHERE Number = 6" in
  let names1 = {name="Name"; vals=[VString "Wang"; VString "Grant"]; typ=TString} in
  let testdb1 = [{title="Phonebook"; cols=[names1;nums]}] in
  let t1 = test1 = testdb1 in
  (* input of update with adding of columns*)
  let test2 = eval db_update "update Phonebook SET Number = Number + 1 WHERE Name = 'Arthur'" in
  let number2 = {name="Number"; vals=[VInt 7; VInt 5]; typ=TInt} in
  let testdb2 = [{title="Phonebook"; cols = [names;number2]}] in
  let t2 = test2 = testdb2 in
  (* input of update with where as a wild card
  let test3 = eval db_update "update Phonebook SET Number = 7 WHERE Name like '%ur'" in
  let number3 = {name="Number"; vals=[VInt 7; VInt 5]; typ=TInt} in
  let testdb3 = [{title="Phonebook"; cols = [names;number3]}] in
  let t3 = test3 = testdb3 in*)
  (* input where column does not exist *)
  let t4 = try let _ = eval db_update "update Phonebook SET Number = 7 WHERE Cheese = 8"
                  in false
              with _ -> true in
  (* test without SET*)
  let t5 = try let _ = eval db_update "update Phonebook where Number = 6" in false
            with _ -> true in
  (* test without where *)
  let t6 = try let _ = eval db_update "update Phonebook SET Number = 6" in false
            with _ -> true in
  t6 && t5 && t4 && t2  && t1

let delnames = {name="Name"; vals=[VString "Arthur"; VString "Arthur";
  VString "Grant"]; typ=TString}
let delnums = {name="Number"; vals=[VInt 5; VInt 6; VInt 5]; typ=TInt}

let deldb = [{title="Phonebook"; cols=[delnames; delnums]}]

TEST "DeleteAND" = (eval db "delete from Phonebook where Name = 'Arthur' AND Number = 5") =
  ([{title="Phonebook";cols=[{name="Name"; vals=[VString "Arthur"; VString "Grant"];
    typ=TString}; {name="Number"; vals=[VInt 6; VInt 5];typ=TInt}]}])

TEST "DeleteOR" = (eval db "delete from Phonebook where Name = 'Arthur' OR Number = 5") =
  ([{title="Phonebook";cols=[{name="Name"; vals=[];
    typ=TString}; {name="Number"; vals=[];typ=TInt}]}])

let drop_db = [{title="Customers";cols=[{name="CustomerID";vals=[VInt(1);VInt(2);
              VInt(3);VInt(4);VInt(5)];typ=TInt};{name="CustomerName";
              vals=[VString("Alfreds Futterkiste");VString("Ana Trujillo
              Emparedados y helados");VString("Antonio Moreno Taqueria");
              VString("Around the Horn");VString("Berglunds snabbkop")];typ=TString};
              {name="ContactName";vals=[VString("Maria Anders");VString
              ("Ana Trujillo");VString("Antonio Moreno");VString("Thomas Hardy");
              VString("Christina Berglund")];
              typ=TString}]};{title="Phonebook"; cols=[names; nums]}]

TEST "Drop" = let new_db = eval drop_db "drop table Customers" in
  let dropped_db = [{title="Phonebook"; cols=[names; nums]}] in
  let _ = new_db = dropped_db in
  let next_db = eval new_db "drop table Phonebook" in
  next_db = []

let col1 = {name = "Name"; vals = [VString "Grant Stento"; VString "Kristy Liao"];
            typ = TString}
let col2 = {name = "Phone_Number"; vals = [VInt 456; VInt 654]; typ = TInt}

let db_alter = [{title = "Phone_book"; cols = [col1;col2]}]

TEST "Alter_Add" = let new_db = eval db_alter "alter table Phone_book ADD Address string" in
  let col3 = {name = "Address"; vals = [VNull; VNull]; typ = TString} in
  let add_db = [{title = "Phone_book"; cols = [col1;col2;col3]}] in
  new_db = add_db

TEST "Alter Drop" = let new_db = eval db_alter "alter table Phone_book DROP COLUMN Name" in
  let drop_db = [{title = "Phone_book"; cols = [col2]}] in
  new_db = drop_db

TEST "Alter Modify with correct type" =
  let new_db = eval db_alter "alter table Phone_book MODIFY COLUMN Phone_Number float" in
  let col3 = {name = "Phone_Number"; vals = [VFloat 456.; VFloat 654.]; typ = TFloat} in
  let modify_db = [{title = "Phone_book"; cols = [col1;col3]}] in
  modify_db = new_db

TEST "Alter Modify with incorrect type" =
  let new_db = eval db_alter "alter table Phone_book MODIFY COLUMN Name boolean" in
  let col3 = {name = "Name"; vals = [VNull; VNull]; typ = TBool} in
  let modify_db = [{title = "Phone_book"; cols = [col3;col2]}] in
  new_db = modify_db

(* Tests for exceptions *)
TEST "Select with wrong column name" =
  try let _ = eval db "select Phonebook from Phonebook" in false
  with _ -> true

TEST "Select with wrong table name" =
  try let _ = eval db "select Name from Number" in false
  with _ -> true

TEST "InsertException" = failwith ""