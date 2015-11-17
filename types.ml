type value =
  | VString of string
  | VInt of int
  | VFloat of float

type val_type =
  | TString
  | TInt
  | TFloat

type category = {name : string; vals : value list; typeof : val_type}
type table = {name : string; cats : category list}
type db = table list