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

(* value comparison functions *)

let val_eq (a:value) (b:value) : bool =
  match a, b with
  | VString x, VString y -> x=y
  | VInt x, VInt y -> x=y
  | VInt x, VFloat y -> (float_of_int x)=y
  | VFloat x, VInt y -> x=(float_of_int y)
  | VFloat x, VFloat y -> x=y
  | VBool x, VBool y -> x=y
  | _, _ -> false

let val_neq (a:value) (b:value) : bool =
  match a, b with
  | VString x, VString y -> x<>y
  | VInt x, VInt y -> x<>y 
  | VInt x, VFloat y -> (float_of_int x)<>y
  | VFloat x, VInt y -> x<>(float_of_int y)
  | VFloat x, VFloat y -> x<>y
  | VBool x, VBool y -> x<>y
  | _, _ -> false

let val_gt (a:value) (b:value) : bool =
  match a, b with
  | VString x, VString y -> x>y
  | VInt x, VInt y -> x>y
  | VInt x, VFloat y -> (float_of_int x)>y
  | VFloat x, VInt y -> x>(float_of_int y)
  | VFloat x, VFloat y -> x>y
  | VBool x, VBool y -> x>y
  | _, _ -> false

let val_lt (a:value) (b:value) : bool =
  match a, b with
  | VString x, VString y -> x<y
  | VInt x, VInt y -> x<y
  | VInt x, VFloat y -> (float_of_int x)<y
  | VFloat x, VInt y -> x<(float_of_int y)
  | VFloat x, VFloat y -> x<y
  | VBool x, VBool y -> x<y
  | _, _ -> false

let val_geq (a:value) (b:value) : bool =
  match a, b with
  | VString x, VString y -> x>=y
  | VInt x, VInt y -> x>=y
  | VInt x, VFloat y -> (float_of_int x)>=y
  | VFloat x, VInt y -> x>=(float_of_int y)
  | VFloat x, VFloat y -> x>=y
  | VBool x, VBool y -> x>=y
  | _, _ -> false

let val_leq (a:value) (b:value) : bool =
  match a, b with
  | VString x, VString y -> x<=y
  | VInt x, VInt y -> x<=y
  | VInt x, VFloat y -> (float_of_int x)<=y
  | VFloat x, VInt y -> x<=(float_of_int y)
  | VFloat x, VFloat y -> x<=y
  | VBool x, VBool y -> x<=y
  | _, _ -> false

(* value combination functions *)

let val_plus (a:value) (b:value) : value =
  match a, b with
  | VInt x, VInt y -> VInt (x + y)
  | VInt x, VFloat y -> VFloat ((float_of_int x) +. y)
  | VFloat x, VInt y -> VFloat (x +. (float_of_int y))
  | VFloat x, VFloat y -> VFloat (x +. y)
  | x, VNull | VNull, x -> VNull
  | _, _ -> failwith "can't add these value types"

let val_minus (a:value) (b:value) : value =
  match a, b with
  | VInt x, VInt y -> VInt (x - y)
  | VInt x, VFloat y -> VFloat ((float_of_int x) -. y)
  | VFloat x, VInt y -> VFloat (x -. (float_of_int y))
  | VFloat x, VFloat y -> VFloat (x -. y)
  | x, VNull | VNull, x -> VNull
  | _, _ -> failwith "can't subtract these value types"

let val_times (a:value) (b:value) : value =
  match a, b with
  | VInt x, VInt y -> VInt (x * y)
  | VInt x, VFloat y -> VFloat ((float_of_int x) *. y)
  | VFloat x, VInt y -> VFloat (x *. (float_of_int y))
  | VFloat x, VFloat y -> VFloat (x *. y)
  | x, VNull | VNull, x -> VNull
  | _, _ -> failwith "can't multiply these value types"

let val_divide (a:value) (b:value) : value =
  match a, b with
  | VInt x, VInt y -> VInt (x / y)
  | VInt x, VFloat y -> VFloat ((float_of_int x) /. y)
  | VFloat x, VInt y -> VFloat (x /. (float_of_int y))
  | VFloat x, VFloat y -> VFloat (x /. y)
  | x, VNull | VNull, x -> VNull
  | _, _ -> failwith "can't divide these value types"

let val_concat (a:value) (b:value) : value =
  match a, b with
  | VString x, VString y -> VString (x ^ y)
  | x, VNull | VNull, x -> VNull
  | _, _ -> failwith "can't concatenate these value types"

let val_and (a:value) (b:value) : value =
  match a, b with
  | VBool x, VBool y -> VBool (x && y)
  | x, VNull | VNull, x -> VNull
  | _, _ -> failwith "can't and these value types"

let val_or (a:value) (b:value) : value =
  match a, b with
  | VBool x, VBool y -> VBool (x || y)
  | x, VNull | VNull, x -> VNull
  | _, _ -> failwith "can't or these value types"

(* value conversion functions *)

let val_to_string : value -> string = function
  | VNull -> "%NaN%"
  | VInt x -> string_of_int x
  | VBool x -> string_of_bool x
  | VFloat x -> string_of_float x
  | VString x -> "'" ^ x ^ "'"

let type_to_string : val_type -> string = function
  | TInt -> "integer"
  | TBool -> "boolean"
  | TFloat -> "float"
  | TString -> "string"

let string_to_type (str:string) : val_type =
  match S.lowercase str with
  | "integer" -> TInt
  | "boolean" -> TBool
  | "float"   -> TFloat
  | "string"  -> TString
  | _         -> failwith "invalid type"

let string_to_val (str:string) (typ:val_type) : value =
  if str = "%NaN%" then VNull else
  let string_check str =
    if str.[0] = '\'' && str.[S.length str - 1] = '\''
    then 
      let sub = S.sub str 1 (S.length str - 2) in
      if not (S.contains sub '\'') then VString sub
      else failwith "single quotes are not allowed inside strings"
    else failwith "single quotes must border strings" in
  let convert_f =
    match typ with
    | TInt -> (fun x -> VInt (int_of_string x))
    | TBool -> (fun x -> VBool (bool_of_string x))
    | TFloat -> (fun x -> VFloat (float_of_string x))
    | TString -> string_check in
  try (convert_f str) with
  | Failure x -> failwith x
  | _ -> failwith "invalid value"

let val_to_val (typ:val_type) (vl:value) : value =
  match vl with
  | VInt a -> if typ = TInt then vl
              else if typ = TFloat then VFloat (float_of_int a)
              else failwith "invalid value type"
  | VBool _ -> if typ = TBool then vl
               else failwith "invalid value type"
  | VFloat _ -> if typ = TFloat then vl
               else failwith "invalid value type"
  | VString _ -> if typ = TString then vl
                 else failwith "invalid value type"
  | VNull -> VNull

let val_to_vstring (value:value) : value =
  match value with
  | VInt x -> VString(string_of_int (x))
  | VBool x -> VString(string_of_bool (x))
  | VFloat x -> VString(string_of_float (x))
  | VString x -> value
  | VNull -> VNull

let val_to_vint (value:value) : value =
  match value with
  | VInt x -> value
  | VFloat x -> VInt(int_of_float x)
  | VString x -> (try VInt(int_of_string x) with _ -> VNull)
  | VBool _ | VNull -> VNull

let val_to_vfloat (value:value) : value =
  match value with
  | VInt x -> VFloat (float_of_int x)
  | VFloat x -> value
  | VString x -> (try VInt(int_of_string x) with _ -> VNull)
  | VBool _ | VNull -> VNull

let val_to_vbool (value:value) : value =
  match value with
  | VBool x -> value
  | VString x -> (try VInt(int_of_string x) with _ -> VNull)
  | VFloat _ | VInt _ | VNull -> VNull