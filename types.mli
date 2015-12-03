module S = String

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

type op =
  | Sp
  | Cma
  | Pl
  | Pr
  | Eq
  | Gt
  | Lt
  | Pls
  | Mns
  | Tms
  | Div
  | Cnct
  | And
  | Or
  | Bt
  | Lk
  | In
  | Nl
  | NNl

(* value comparison functions *)

val val_eq : value -> value -> bool

val val_neq : value -> value -> bool

val val_gt : value -> value -> bool

val val_lt : value -> value -> bool

val val_geq : value -> value -> bool

val val_leq : value -> value -> bool

(* value combination functions *)

val val_plus : value -> value -> value

val val_minus : value -> value -> value

val val_times : value -> value -> value

val val_divide : value -> value -> value

val val_concat : value -> value -> value

val val_and : value -> value -> value

val val_or : value -> value -> value

(* value conversion functions *)

val val_to_string : value -> string

val type_to_string : val_type -> string

val string_to_type : string -> val_type

val string_to_val : string -> val_type -> value

val val_to_val : val_type -> value -> value

val val_to_vstring : value -> value

val val_to_vint : value -> value

val val_to_vfloat : value -> value

val val_to_vbool : value -> value