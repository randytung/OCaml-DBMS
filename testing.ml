open Types
open Operations

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
TEST "Insert" = failwith ""
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