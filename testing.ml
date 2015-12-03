open Types
open Operations
open Assertions

(* Tests for accuracy *)
(*demodb is an example database taken from http://www.w3schools.com/sql/sql_select.asp*)
let demodb = [{title="Customers";cols=[{name="CustomerID";vals=[VInt(1);VInt(2);VInt(3);VInt(4);VInt(5)];typ=TInt};
              {name="CustomerName";vals=[VString("Alfreds Futterkiste");VString("Ana Trujillo Emparedados y helados");VString("Antonio Moreno Taqueria");VString("Around the Horn");VString("Berglunds snabbkop")];typ=TString};
              {name="ContactName";vals=[VString("Maria Anders");VString("Ana Trujillo");VString("Antonio Moreno");VString("Thomas Hardy");VString("Christina Berglund")];typ=TString};
              {name="Address";vals=[VString("Obere Str. 57");VString("Avda. de la Constitucion 2222");VString("Mataderos 2312");VString("120 Hanover Sq.");VString("Berguvsvagen 8")];typ=TString};
              {name="City";vals=[VString("Berlin");VString("Mexico D.F.");VString("Mexico D.F.");VString("London");VString("Lulea")];typ=TString};
              {name="PostalCode";vals=[VString("12209");VString("05021");VString("05023");VString("WA1 1DP");VString("S-958 22")];typ=TString}]}]
TEST "Select" = (select (demodb) "CustomerName,City FROM Customers") = demodb



let names = {name="Name"; vals=[VString "Arthur"; VString "Grant"]; typ=TString}
let nums = {name="Number"; vals=[VInt 6; VInt 5]; typ=TInt}

let db = [{title="Phonebook"; cols=[names; nums]}]


TEST "Create" =
  (*test with correct input*)
  let db_tbl = create db "Student (Name string,netid string)" in
  let name =  {name="Name"; vals=[]; typ=TString} in
  let netid = {name="netid"; vals=[]; typ=TString} in
  let check_db = [{title="Student"; cols = [netid;name]};{title="Phonebook";
    cols=[names; nums]} ] in
  let _ = check_db = db_tbl in
  (*test with empty columns*)
  let db_tbl2 = create db "People ()" in
    let check_db2 = [{title="People"; cols = []};{title="Phonebook";
    cols = [names; nums]}] in
  let _ = check_db2 = db_tbl2 in
  (*testing with existing table name*)
  let test1 = try let _ = create db "Phonebook ()" in false
              with _ -> true in
  let _ = test1 in
  (*testing with nonexisting type*)
  let test2 = try let _  = create db "Person (Name int, Number cheese)" in false
              with _ -> true in
  test2

let add_val col v =
  let new_vals = col.vals @ [v] in
  {name=col.name; vals=new_vals; typ=col.typ}

TEST "Insert normally" =
  let db' = insert db "Phonebook VALUES ('Randy', 7)" in
  let names' = add_val names (VString "Randy") in
  let nums' = add_val nums (VInt 7) in
  db' = [{title="Phonebook"; cols=[names'; nums']}]

TEST "Insert with spaces" =
  let db' = insert db "Phonebook VALUES ('Kristy  ', 2)" in
  let names' = add_val names (VString "Kristy  ") in
  let nums' = add_val nums (VInt 2) in
  db' = [{title="Phonebook"; cols=[names'; nums']}]

TEST "Insert 1 value" =
  let db' = insert db "Phonebook (Name) VALUES ('Ellen')" in
  let names' = add_val names (VString "Ellen") in
  let nums' = add_val nums (VNull) in
  db' = [{title="Phonebook"; cols=[names'; nums']}]

TEST "Insert with 2 specified columns" =
  let db' = insert db "Phonebook (Number, Name) VALUES (9, 'Donald')" in
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
  let db' = insert big_db ("Stuffs (Stuff 2, Stuff 5, Stuff 4, Stuff 3) " ^
            "VALUES (1, 3.4, true, false)") in
  let stuff1' = add_val stuff1 VNull in
  let stuff3' = add_val stuff3 (VBool false) in
  let stuff2' = add_val stuff2 (VInt 1) in
  let stuff5' = add_val stuff5 (VFloat 3.4) in
  let stuff4' = add_val stuff4 (VBool true) in
  db' = [{title="Stuffs"; cols=[stuff1'; stuff2'; stuff3'; stuff4'; stuff5']}]

TEST "Insert certain columns without specifying column names" =
  try let _  = insert db "Phonebook VALUES ('Ellen')" in false
  with _ -> true

TEST "Insert without values" =
  try let _  = insert db "Phonebook ('Ellen')" in false
  with _ -> true

TEST "Insert uneven number of columns and values" =
  try let _  = insert db "Phonebook (Name, Number) VALUES ('Ellen')" in false
  with _ -> true

TEST "Update" = failwith ""

TEST "DeleteAND" = (delete demodb "Customers
  WHERE CustomerName='Alfreds Futterkiste' OR ContactName='Christina Berglund'") =
  ([{title="Customers";cols=[{name="CustomerID";vals=[VInt(2);VInt(3);VInt(4);VInt(5)];typ=TInt};
  {name="CustomerName";vals=[VString("Ana Trujillo Emparedados y helados");VString("Antonio Moreno Taqueria");VString("Around the Horn");VString("Berglunds snabbkop")];typ=TString};
  {name="ContactName";vals=[VString("Ana Trujillo");VString("Antonio Moreno");VString("Thomas Hardy");VString("Christina Berglund")];typ=TString};
  {name="Address";vals=[VString("Avda. de la Constitucion 2222");VString("Mataderos 2312");VString("120 Hanover Sq.");VString("Berguvsvagen 8")];typ=TString};
  {name="City";vals=[VString("Mexico D.F.");VString("Mexico D.F.");VString("London");VString("Lulea")];typ=TString};
  {name="PostalCode";vals=[VString("05021");VString("05023");VString("WA1 1DP");VString("S-958 22")];typ=TString}]}]);

TEST "DeleteOR" = (delete demodb "Customers
  WHERE CustomerName='Alfreds Futterkiste' AND ContactName='Maria Anders'") =
  ([{title="Customers";cols=[{name="CustomerID";vals=[VInt(2);VInt(3);VInt(4)];typ=TInt};
  {name="CustomerName";vals=[VString("Ana Trujillo Emparedados y helados");VString("Antonio Moreno Taqueria");VString("Around the Horn")];typ=TString};
  {name="ContactName";vals=[VString("Ana Trujillo");VString("Antonio Moreno");VString("Thomas Hardy")];typ=TString};
  {name="Address";vals=[VString("Avda. de la Constitucion 2222");VString("Mataderos 2312");VString("120 Hanover Sq.")];typ=TString};
  {name="City";vals=[VString("Mexico D.F.");VString("Mexico D.F.");VString("London")];typ=TString};
  {name="PostalCode";vals=[VString("05021");VString("05023");VString("WA1 1DP")];typ=TString}]}])

let drop_db = [{title="Customers";cols=[{name="CustomerID";vals=[VInt(1);VInt(2);VInt(3);VInt(4);VInt(5)];typ=TInt};
              {name="CustomerName";vals=[VString("Alfreds Futterkiste");VString("Ana Trujillo Emparedados y helados");VString("Antonio Moreno Taqueria");VString("Around the Horn");VString("Berglunds snabbkop")];typ=TString};
              {name="ContactName";vals=[VString("Maria Anders");VString("Ana Trujillo");VString("Antonio Moreno");VString("Thomas Hardy");VString("Christina Berglund")];
              typ=TString}]};{title="Phonebook"; cols=[names; nums]}]


TEST "Drop" = let new_db = drop drop_db "Customers" in
  let dropped_db = [{title="Phonebook"; cols=[names; nums]}] in
  let _ = new_db = dropped_db in
  let next_db = drop new_db "Phonebook" in
  next_db = []


let col1 = {name = "Name"; vals = [VString "Grant Stento"; VString "Kristy Liao"];
       typ = TString}
let col2 = {name = "Phone_Number"; vals = [VInt 456; VInt 654]; typ = TInt}

let db = [{title = "Phone_book"; cols = [col1;col2]}]

TEST "Alter_Add" = let new_db = alter db "Phone_book ADD Address string" in
  let col3 = {name = "Address"; vals = [VNull; VNull]; typ = TString} in
  let add_db = [{title = "Phone_book"; cols = [col3;col1;col2]}] in
  new_db = add_db

TEST "Alter Drop" = let new_db = alter db "Phone_book DROP COLUMN Name" in
  let drop_db = [{title = "Phone_book"; cols = [col2]}] in
  new_db = drop_db

TEST "Alter Modify with correct type" =
  let new_db = alter db "Phone_book MODIFY COLUMN Phone_Number float" in
  let col3 = {name = "Phone_Number"; vals = [VFloat 456.; VFloat 654.]; typ = TFloat} in
  let modify_db = [{title = "Phone_book"; cols = [col1;col3]}] in
  modify_db = new_db

TEST "Alter Modify with incorrect type" =
  let new_db = alter db "Phone_book MODIFY COLUMN Name boolean" in
  let col3 = {name = "Name"; vals = [VNull; VNull]; typ = TBool} in
  let modify_db = [{title = "Phone_book"; cols = [col3;col2]}] in
  new_db = modify_db