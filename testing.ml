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
TEST "Create" = failwith ""

let names = {name="Name"; vals=[VString "Arthur"; VString "Grant"]; typ=TString}
let nums = {name="Number"; vals=[VNull; VInt 6; VInt 5]; typ=TInt}

let db = [{title="Phonebook"; cols=[names; nums]}]

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
  let db' = insert big_db "Stuffs (Stuff 2, Stuff 5, Stuff 4, Stuff 3) VALUES (1, 3.4, true, false)" in
  let stuff1' = add_val stuff1 VNull in
  let stuff3' = add_val stuff3 (VBool false) in
  let stuff2' = add_val stuff2 (VInt 1) in
  let stuff5' = add_val stuff5 (VFloat 3.4) in
  let stuff4' = add_val stuff4 (VBool true) in
  db' = [{title="Stuffs"; cols=[stuff1'; stuff2'; stuff3'; stuff4'; stuff5']}]

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
  {name="PostalCode";vals=[VString("05021");VString("05023");VString("WA1 1DP")];typ=TString}]}]);
TEST "Drop" = failwith ""
TEST "Alter" = failwith ""

(* Tests for exceptions *)
TEST_UNIT "Select" = failwith ""
TEST_UNIT "Insert" = failwith ""
