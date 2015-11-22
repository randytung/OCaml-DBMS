type value =
  | VNull
  | VInt of int
  | VBool of bool
  | VFloat of float
  | VString of string

type val_type =
  | TInt
  | TBool
  | TFloat
  | TString

type column = {name : string; vals : value list; typ : val_type}
type table = {title : string; cols : column list}
type db = table list