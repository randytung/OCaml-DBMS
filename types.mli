(** the type of a category *)
type val_type =
  | String of string
  | Integer of int
  | Float of float

type category = {name : string; vals : 'a list; type : val_type}
type table = {name : string; cats : category list}
type db = table list