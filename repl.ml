open Operations
open Builder
open Types

let help () =
  let line = ("--------------------------------------------------------------" ^
              "------------------\n") in
  let sel =
    ("(1) SELECT: prints specified data from the database. \n" ^ "Syntax: \n" ^
    "SELECT column_name1,column_name2,... FROM table_name; \n or \n" ^
    "SELECT * FROM table_name; \n or \nSELECT ... FROM table_name " ^
    "WHERE ...; \n ***See very end for more details about WHERE. \n" ^ line) in

  let create =
    ("(2) CREATE TABLE: creates a new table in the database. \n" ^ "Syntax:\n" ^
    "CREATE TABLE table_name; \n" ^ line) in

  let insert =
    ("(3) INSERT INTO: inserts a new row into a table. \n" ^ "Syntax: \n" ^
    "INSERT INTO table_name VALUES (value1,value2,value3,...); \n or \n" ^
    "INSERT INTO table_name (column1,column2,column3,...) " ^
    "VALUES (value1,value2,value3,...); \n" ^ line) in

  let update =
    ("(4) UPDATE: updates specified data in a table. \n" ^ "Syntax: \n" ^
    "UPDATE table_name SET column1=value1,column2=value2,... " ^
    "WHERE ...; \n" ^ "***Other forms of SET: \nSET value=column \n or \n" ^
    "SET column=value1+value2 (and other primitive operations on values) \n" ^
    "***See very end for more details about WHERE. \n" ^ line) in

  let delete =
    ("(5) DELETE FROM: deletes row(s) from a table. \n" ^ "Syntax: \n" ^
    "DELETE FROM table_name WHERE some_column=some_value; \n" ^
    "***See very end for more details about WHERE. \n" ^ line) in

  let drop =
    ("(6) DROP TABLE: deletes table from the database. \n" ^ "Syntax: \n" ^
    "DROP TABLE table_name; \n" ^ line) in

  let alter =
    ("(7) ALTER TABLE: adds, deletes, and modifies columns in a table. \n" ^
    "Syntax: \n" ^ "ALTER TABLE table_name ADD column_name datatype; \n or \n" ^
    "ALTER TABLE table_name DROP COLUMN column_name; \n or \n" ^
    "ALTER TABLE table_name MODIFY COLUMN column_name datatype; \n" ^ line) in

  let where =
    ("(*) WHERE: returns specified data based on provided restrictions. \n" ^
    "Supports the following operations: =, <>, >, <, >=, <=, BETWEEN, " ^
    "LIKE, IN \n" ^ "Wildcards: \n% (matches 0 or more characters) \n" ^
    "_ (matches a single character) \n[chars] (matches character in sets " ^
    "and ranges of characters) \n[!chars] (matches character NOT " ^
    "specified within brackets) \nExamples: \nSELECT * FROM Customers " ^
    "WHERE City LIKE 'ber%'; \nSELECT * FROM Customers WHERE City LIKE " ^
    "'[a-c]%'; \nSELECT * FROM Customers WHERE City IN ('Paris','London'); \n" ^
    "SELECT * FROM Products WHERE Price BETWEEN 10 AND 20; \nSELECT * FROM " ^
    "Customers WHERE CustomerID=1; \n" ^ line) in

  (* Examples for where taken from:
     http://www.w3schools.com/sql/sql_between.asp
     http://www.w3schools.com/sql/sql_in.asp
     http://www.w3schools.com/sql/sql_wildcards.asp
     http://www.w3schools.com/sql/sql_where.asp *)

  print_string (line ^ sel ^ create ^ insert ^ update ^ delete ^
                drop ^ alter ^ where)

let rec read_lines acc =
  let s = read_line () in
  let new_acc =
    if acc = "" then s else acc^" "^s in
  if (try s.[String.length s - 1] = ';' with _ -> false)
  then S.sub new_acc 0 (S.length new_acc - 1)
  else read_lines new_acc

let rec loop (db:db) (file_name:string) : unit =
  print_string ("Enter a command, type \"help;\" for a list of commands, " ^
                "or type \"quit;\" to exit.\n");
  let input = read_lines "" in
  match String.lowercase input with
  | "help"  -> help(); loop db file_name
  | "quit"  -> save db file_name
  | _       -> (let new_db =
                    try (let x = eval db input in
                         print_string "Success!\n"; x)
                    with
                    | Failure msg -> print_string ("Error: " ^ msg ^
                                                   "\nPlease try again.\n"); db
                    | _ -> print_string "Error. Please try again.\n"; db in
                save new_db file_name;
                loop new_db file_name)