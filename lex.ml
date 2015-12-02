open Types
open Basic_parse

let to_op = function
  | " " -> Sp
  | "," -> Cma
  | "(" -> Pl
  | ")" -> Pr
  | "=" -> Eq
  | ">" -> Gt
  | "<" -> Lt
  | "+" -> Pls
  | "-" -> Mns
  | "*" -> Tms
  | "/" -> Div
  | "^" -> Cnct
  | "or" -> Or
  | "and" -> And
  | "between" -> Bt
  | "like" -> Lk
  | "in" -> In
  | _ -> failwith "parser error - not an op"

let dels = [" "; ","; "("; ")"; "="; "+"; "-";"*"; "/"; "and"; "or"]

let lex str dels =
  let qts = chr_split_lst str '\'' false in
  let rec get_strs lst =
    match lst with
    | [] -> failwith "extra quote"
    | h::[] -> (`Othr h)::[]
    | h1::h2::t -> (`Othr h1)::(`Str h2)::(get_strs t) in
  let qt_splt = get_strs qts in
  let rec get_ops str d =
    match str_split str d with
    | None -> [`Othr str]
    | Some ("","") -> (`Op (to_op d))::[]
    | Some ("",a) -> (`Op (to_op d))::(get_ops a d)
    | Some (b,"") -> (`Othr b)::(`Op (to_op d))::[]
    | Some (b,a) -> (`Othr b)::(`Op (to_op d))::(get_ops a d) in
  let rec lex_del lst del =
    match lst with
    | [] -> []
    | (`Str h)::t -> [`Str h]::(lex_del t del)
    | (`Op h)::t -> [`Op h]::(lex_del t del)
    | (`Othr h)::t -> (get_ops h del)::(lex_del t del) in
  let del_splt = List.fold_left (fun x y -> List.flatten (lex_del x y)) qt_splt dels in
  let blank_filter = function
    | `Str _ -> true
    | `Op a -> a <> Sp
    | `Othr a -> a <> "" in
  let blnk_splt = List.filter blank_filter del_splt in
  let value_finder = function
    | `Op a -> `Op a
    | `Str a -> `Val (VString a)
    | `Othr a -> try `Val (VInt (int_of_string a)) with _ ->
                 try `Val (VFloat (float_of_string a)) with _ ->
                 try `Val (VBool (bool_of_string a)) with _ ->
                 `Col a in
  let tokens = List.map value_finder blnk_splt in
  let rec neg_num_filter lst =
    match lst with
    | (`Op a)::(`Op Mns)::(`Val (VInt i))::t ->
      (`Op a)::(`Val (VInt (-i)))::(neg_num_filter t)
    | (`Op a)::(`Op Mns)::(`Val (VFloat f))::t ->
      (`Op a)::(`Val (VFloat (-.f)))::(neg_num_filter t)
    | h::t -> h::(neg_num_filter t)
    | [] -> [] in
  neg_num_filter tokens