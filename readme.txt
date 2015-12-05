Hi! This final project is a database management system created by
Arthur Wang, Grant Stento, Kristy Liao, and Randy Tung. Our database involves
the basic functionality of a regular database from SQL. In order to compile
our database, you must input

cs3110 compile -p yojson main.ml

in the command line where all of the files are located. After it compiles, type

cs3110 run main.ml

in order to run the database. It will ask for an existing database, which must
be in the form of a json. If you do not have a database, it will create one
for you. In order to learn how to use the database, type "help" inside the REPL to see a list of commands that you can do in our database. Experiment, and have fun!

If you want to run our test cases, type in

cs3110 compile testing.ml
cs3110 test testing.ml

