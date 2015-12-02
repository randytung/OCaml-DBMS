open Types

(* return a column from [cols] with name [name], or fail if one doesn't exist *)
let get_col (cols:column list) (name:string) : column =
  try List.find (fun x -> x.name = name) cols
  with _ -> failwith "invalid col name"

(* return a list of vals with the length of values in [cols]: all the same val
 * if [val_col] is `V, or the vals in a column if [val_col] is `C*)
let get_vals val_col (cols:column list) : value list =
  match val_col with
  | `V v -> (try List.map (fun x -> v) (List.hd cols).vals with _ -> [])
  | `C c -> (get_col cols c).vals

(* fails if first token in [tokens] is not marked as a `Val or `Col
 * if marked as `Val a, returns `V a
 * if marked as `Col a, fails if a isn't found in [cols], or returns `C a *)
let next_val_col (cols:column list) tokens =
  match tokens with
  | (`Op a)::t -> failwith "parse error: expected value or column, got operator"
  | (`Val a)::t -> (`V a, t)
  | (`Col a)::t -> if List.exists (fun x -> x.name = a) cols then (`C a, t)
                   else failwith ("parse error: " ^ a ^
                                  " is not a valid column or value")
  | _ -> failwith "parse error"

(* Parse Chain *)

(* break off any parens from [tokens] before calling parse_ops *)
let rec parse_parens (cols:column list) tokens =
  match tokens with
  | (`Op Pl)::h::t -> let (vals, tokens_2) =
                        if h = `Op Pl
                        then parse_parens cols (h::t)
                        else
                          let (val_col, tokens_3) = next_val_col cols (h::t) in
                          let vals = get_vals val_col cols in
                          parse_ops vals cols tokens_3 in
                      (match tokens_2 with
                       | (`Op Pr)::t2 -> parse_ops vals cols t2
                       | _ -> failwith "missing closing parenthesis")
  | _ -> let (val_col, tokens_2) = next_val_col cols tokens in
         let vals = get_vals val_col cols in
         parse_ops vals cols tokens_2

(* call an op from [tokens] on [firsts] and
 * a second list of vals from [tokens] *)
(* not tail recursive! *)
and parse_ops (firsts:value list) (cols:column list) tokens =
  let next_op =
    match tokens with
    | (`Op Pls)::t -> Some (val_plus, t)
    | (`Op Mns)::t -> Some (val_minus, t)
    | (`Op Tms)::t -> Some (val_times, t)
    | (`Op Div)::t -> Some (val_divide, t)
    | (`Op Cnct)::t -> Some (val_concat, t)
    | (`Op And)::t -> Some (val_and, t)
    | (`Op Or)::t -> Some (val_or, t)
    | (`Op Cma)::_ | (`Op Pr)::_ | [] -> None
    | _ -> failwith "improper operator placement" in
  match next_op with
  | None -> (firsts, tokens)
  | Some (oper, tokens_2) ->
    let (seconds, tokens_3) = parse_parens cols tokens_2 in
    (List.rev (List.rev_map2 (fun x y -> oper x y) firsts seconds), tokens_3)

(* returns association list between a column name and new values of each column
 * in the set of columns converted from [cols] using [tokens] from the lexer *)
let rec parse (cols:column list) tokens =
  match tokens with
  | (`Col a)::(`Op Eq)::t ->
    let (new_vals, tokens_2) = parse_parens cols t in
    let col = get_col cols a in
    let col_info = (col.name, List.map (val_to_val col.typ) new_vals) in
    (match tokens_2 with
     | (`Op Cma)::t2 -> col_info::(parse cols t2)
     | [] -> [col_info]
     | _ -> failwith "extra closing parentheses")
  | _ -> failwith "parse error"