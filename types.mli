type value =
  | VString of string
  | VInt of int
  | VFloat of float

type val_type =
  | TString
  | TInt
  | TFloat

type category = {name : string; vals : value list; typ : val_type}
type table = {title : string; cats : category list}
type db = table list