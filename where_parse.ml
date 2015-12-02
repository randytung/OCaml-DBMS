open Types

(* Helper Functions *)

(* same as mapi, but is tail recursive *)
let safe_mapi (fn:int -> 'a -> 'b) (lst:'a list) : 'b list =
  let rec s_m f l i acc =
    match l with
    | h::t -> s_m f t (i+1) ((f i h)::acc)
    | [] -> acc in
  List.rev (s_m fn lst 0 [])

(* returns the indices of values in [cols]; empty if [cols] is empty *)
let all_indices (cols:column list) : int list =
    try safe_mapi (fun i x -> i) ((List.hd cols).vals) with _ -> []

(* find a column with [name] in [cols] and return its list of values.
 * assumes column with [name] is already known to be in [cols] *)
let get_col_vals (name:string) (cols:column list) : value list =
  let col = List.find (fun x -> x.name = name) cols in
  col.vals

(* uniquely sorts [lst1] and [lst2] from least to greatest, then returns the
 * union of [lst1] and [lst2] using the set definition of union *)
let union (lst1:int list) (lst2:int list) : int list =
  let rec u a b =
    match a, b with
    | ha::ta, hb::tb -> if ha = hb then ha::(u ta tb) else ha::hb::(u ta tb)
    | _, _ -> a @ b in
  u (List.sort_uniq (-) lst1) (List.sort_uniq (-) lst2)

(* uniquely sorts [lst1] and [lst2] from least to greatest, then returns the
 * intersection of [lst1] and [lst2] using the set definition of intersect *)
let intersect (lst1:int list) (lst2:int list) : int list =
  let rec i a b =
    match a, b with
    | ha::ta, hb::tb -> if ha = hb then ha::(i ta tb)
                        else if ha > hb then i a tb else i ta b
    | _, _ -> [] in
  i (List.sort_uniq (-) lst1) (List.sort_uniq (-) lst2)

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


(* Comparison Parse Functions *)

(* if both [first] and [second] are `V a values, then compare them and
 * match all if comparison succeeds, none if it does not
 * if either or both are `C a columns, compare each pair of values and
 * match each if comparison succeeds, or don't if it does not *)
let parse_compare (comp_func:value -> value -> bool)
                   first second (cols:column list) : int list =
  match first, second with
  | `V a, `V b -> if comp_func a b then all_indices cols else []
  | `V a, `C b -> let b_vals = get_col_vals b cols in
                  let comp_filter i x = if comp_func a x then i else -1 in
                  let filtered_vals = safe_mapi comp_filter b_vals in
                  List.filter (fun i -> i <> -1) filtered_vals
  | `C a, `V b -> let a_vals = get_col_vals a cols in
                  let comp_filter i x = if comp_func x b then i else -1 in
                  let filtered_vals = safe_mapi comp_filter a_vals in
                  List.filter (fun i -> i <> -1) filtered_vals
  | `C a, `C b -> let a_vals = get_col_vals a cols in
                  let b_vals = get_col_vals b cols in
                  let comp_filter i x = if comp_func x (List.nth b_vals i)
                                        then i else -1 in
                  let filtered_vals = safe_mapi comp_filter a_vals in
                  List.filter (fun i -> i <> -1) filtered_vals

(* return indices of values from [cols] where [first] is =
 * a second column or value parsed from [tokens] *)
let parse_eq first (cols:column list) tokens =
   let (second, tokens_2) = next_val_col cols tokens in
   let matches = parse_compare val_eq first second cols in
   (matches, tokens_2)

(* return indices of values from [cols] where [first] is > or >=
 * a second column or value parsed from [tokens] *)
let parse_gt first (cols:column list) tokens =
  match tokens with
  | (`Op Eq)::t -> let (second, tokens_2) = next_val_col cols t in
                   let matches = parse_compare val_geq first second cols in
                   (matches, tokens_2)
  | _ -> let (second, tokens_2) = next_val_col cols tokens in
         let matches = parse_compare val_gt first second cols in
         (matches, tokens_2)

(* return indices of values from [cols] where [first] is < or <= or <>
 * a second column or value parsed from [tokens] *)
let parse_lt first (cols:column list) tokens =
  match tokens with
  | (`Op Eq)::t -> let (second, tokens_2) = next_val_col cols t in
                   let matches = parse_compare val_leq first second cols in
                   (matches, tokens_2)
  | (`Op Gt)::t -> let (second, tokens_2) = next_val_col cols t in
                   let matches = parse_compare val_neq first second cols in
                   (matches, tokens_2)
  | _ ->  let (second, tokens_2) = next_val_col cols tokens in
          let matches = parse_compare val_lt first second cols in
          (matches, tokens_2)

(* return indices of values from [cols] where [first] is between
 * a second and third column or value parsed from [tokens] *)
let parse_bt first (cols:column list) tokens =
  let (second, tokens_2) = next_val_col cols tokens in
  let (third, tokens_3) =
      match tokens_2 with
      | (`Op And)::t -> next_val_col cols t
      | _ -> failwith "missing 'and'" in
  let matches1 = parse_compare val_geq first second cols in
  let matches2 = parse_compare val_leq first third cols in
  (intersect matches1 matches2, tokens_3)

(* turn [str] into a list of chars *)
let explode_string (str:string) : char list =
  let rec expl s i =
    if i = S.length s then [] else s.[i]::(expl s (i+1)) in
  expl str 0

(* return an option: None where [str] does not appear in match, or
 * Some a where a is the index of [str] where it matches [mtch] *)
let match_index (mtch:string) (str:string) : int option =
  let rec m_i m s i =
    if S.length m > S.length s - i then None
    else if S.sub s i (S.length m) = m then Some i
    else if S.contains_from s (i+1) m.[0]
         then m_i m s (S.index_from s (i+1) m.[0])
         else None in
  if mtch = "" then None else m_i mtch str 0

(* returns a range of chars from [start] to [finish] according
 * to the ocaml order of chars *)
let char_range (start:char) (finish:char) : char list =
  let start_n = int_of_char start in
  let finish_n = int_of_char finish in
  let rec builder s f = 
    if s = f then [char_of_int s]
    else (char_of_int s)::(builder (s+1) f) in
  builder start_n finish_n

(* move through [mtch_str] and [word], checking if they match
 * piece by piece, split by [wldcrds] *)
let rec lk_matcher (mtch_str:string) (word:string)
                   (wldcrds:char list) : bool =
  match wldcrds with
  | [] -> mtch_str = word
  | wc::tl -> let wc_i = String.index mtch_str wc in
              if S.sub mtch_str 0 wc_i <> S.sub word 0 wc_i then false
              else
                match wc with
                | '%' -> percent_match mtch_str word wc_i tl
                | '_' -> underscore_match mtch_str word wc_i tl
                | '[' -> bracket_match mtch_str word wc_i tl
                | _ -> failwith ("can't have closing bracket " ^
                                 "without an opening bracket")

(* match [mtch_str] and [word] immediately after '%' at location [wc_i] *)
and percent_match (mtch_str:string) (word:string)
                  (wc_i:int) (wldcrds:char list) : bool  =
  match wldcrds with
  | [] -> true
  | next_wc::tl ->
    let next_i = String.index_from mtch_str (wc_i + 1) next_wc in
    let str_btwn = S.sub mtch_str (wc_i + 1) (next_i - wc_i - 1) in
    match match_index str_btwn (S.sub word wc_i (S.length word - wc_i)) with
    | None -> false
    | Some i_aftr_prcnt ->
      let mtch_rest = (S.sub mtch_str next_i (S.length mtch_str - next_i)) in
      let word_rest =
          (S.sub word (wc_i + i_aftr_prcnt + (S.length str_btwn))
          (S.length word - i_aftr_prcnt - wc_i - (S.length str_btwn))) in
      lk_matcher mtch_rest word_rest wldcrds

(* match [mtch_str] and [word] immediately after '_' at location [wc_i] *)
and underscore_match (mtch_str:string) (word:string)
                     (wc_i:int) (wldcrds:char list) : bool =
  try lk_matcher (S.sub mtch_str (wc_i + 1) (S.length mtch_str - wc_i - 1))
                 (S.sub word (wc_i + 1) (S.length word - wc_i - 1)) wldcrds
  (* catch an isolated error when matching strings
   * with oddly-placed underscores *)
  with Invalid_argument "String.sub / Bytes.sub" -> false

(* match [mtch_str] and [word] between '[' and ']' starting at
 * location [wc_i] by turning the string in the middle into a char list *)
and bracket_match (mtch_str:string) (word:string)
                  (wc_i:int) (wldcrds:char list) : bool =
  match wldcrds with
  | ']'::t2 ->
    let end_i = String.index_from mtch_str (wc_i + 1) ']' in
    let chr_lst'' =
        explode_string (S.sub mtch_str (wc_i + 1) (end_i - wc_i - 1)) in
    let (is_neg, chr_lst') =
      match chr_lst'' with
      | [] -> (false, chr_lst'')
      | h::t -> if h = '!' then (true, t) else (false, chr_lst'') in
    let chr_lst =
      match chr_lst' with
      | h1::'-'::h2::[] -> (try char_range h1 h2
                            with _ -> failwith "invalid range")
      | _ -> chr_lst' in
    if List.mem '!' chr_lst then failwith ("! is reserved to be used once " ^
                                           "at the front of a char list")
    else if List.mem '-' chr_lst then failwith "extra '-'; invalid range"
    else
      let all_chars = char_range (char_of_int 0) (char_of_int 255) in
      let opp_chr_lst =
          List.filter (fun x -> not (List.mem x chr_lst)) all_chars in
      let char_list = if is_neg then opp_chr_lst else chr_lst in
      if char_list = []
      then
        lk_matcher (S.sub mtch_str (end_i + 1) (S.length mtch_str - end_i - 1))
                   (S.sub word wc_i (S.length word - wc_i)) t2
      else
        if not (try List.mem word.[wc_i] char_list with _ -> false) then false
        else
         lk_matcher (S.sub mtch_str (end_i + 1) (S.length mtch_str - end_i - 1))
                    (S.sub word (wc_i + 1) (S.length word - wc_i - 1)) t2
  | [] -> failwith "need closing bracket"
  | _ -> failwith "can't have wildcards in char list match"

(* return indices of values from [cols] where [first] equals
 * a second column or value parsed from [tokens] *)
let parse_lk first (cols:column list) tokens =
  let (second, tokens_2) = next_val_col cols tokens in
  let val_is_match vl mtch_str =
    let word = match vl with
              | VString a -> a
              | _ -> failwith "not a string" in
    let chars = explode_string mtch_str in
    let wldcrds = List.filter (fun x -> List.mem x ['%';'_';'[';']']) chars in
    lk_matcher mtch_str word wldcrds in
  let matches =
    match first, second with
    | `V (VString a), `V (VString b) -> if val_is_match (VString a) b
                                        then all_indices cols else []
    | `C a, `V (VString b) -> let rec match_filter i vl =
                                if val_is_match vl b then i else -1 in
                              let vls = get_col_vals a cols in
                              let filtered_vals = safe_mapi match_filter vls in
                              List.filter (fun x -> x <> -1) filtered_vals
    | _, _ -> parse_compare val_eq first second cols in
  (matches, tokens_2)

(* return indices of values from [cols] where [first] equals
 * a second column or value parsed from [tokens] *)
let parse_in first (cols:column list) tokens =
  let rec get_comps (comps, tkns) =
    match tkns with
    | [] -> failwith "missing closing paren"
    | (`Op Pr)::t -> (comps, t)
    | _ -> (let (comp, tokens_2) = next_val_col cols tkns in
            match tokens_2 with
            | (`Op Cma)::t -> get_comps (comp::comps, t)
            | (`Op Pr)::t -> (comp::comps, t)
            | _ -> failwith "missing comma or closing paren") in
  let (comps, tokens_2) =
    match tokens with
    | (`Op Pl)::t -> get_comps ([], t)
    | _ -> failwith "missing comparison list" in
  (* go through all indices corresponding to values.
   * use converter to turn any cols into the value corresponding
   * to the current index, then compare and return i or -1 *)
  let rec in_matcher comps i_lst acc =
    let get_val cols i vl =
      match vl with
      | `C a -> List.nth (List.find (fun x -> x.name = a) cols).vals i
      | `V a -> a in
    match i_lst with
    | h::t -> let vl = get_val cols h first  in
              let vl_comps = List.map (get_val cols h) comps in
              if List.mem vl vl_comps then in_matcher comps t (h::acc)
              else in_matcher comps t acc
    | [] -> acc in
  let matches = List.rev (in_matcher comps (all_indices cols) []) in
  (matches, tokens_2)


(* Parse Chain *)

(* break off any parens from [tokens] before calling parse_ops *)
let rec parse_parens (cols:column list) tokens =
  match tokens with
  | (`Op Pl)::h::t -> let (matches, tokens_2) =
                        if h = `Op Pl
                        then parse_parens cols (h::t)
                        else let (first, tokens_3) = next_val_col cols (h::t) in
                             parse_ops first cols tokens_3 in
                      (match tokens_2 with
                       | (`Op Pr)::t2 -> parse_connectors cols (matches, t2)
                       | _ -> failwith "missing closing parenthesis")
  | _ -> let (first, tokens_2) = next_val_col cols tokens in
         parse_ops first cols tokens_2

(* call an op from [tokens] on [first] and a second col or val from [tokens] *)
and parse_ops first (cols:column list) tokens =
  match tokens with
  | (`Col _)::t
  | (`Val _)::t -> failwith ("parse error: operator required after column name")
  | (`Op a)::t -> let f = match a with
                          | Eq -> parse_eq
                          | Gt -> parse_gt
                          | Lt -> parse_lt
                          | Bt -> parse_bt
                          | Lk -> parse_lk
                          | In -> parse_in
                          | _ -> failwith ("parse error: invalid operator") in
                  parse_connectors cols (f first cols t)
  | _ -> failwith "parse error"

(* connect result [m1] to other results from parse_ops through [tokens_1] *)
and parse_connectors (cols:column list) (m1, tokens_1) =
  match tokens_1 with
  | (`Op And)::t -> let (m2, tokens_2) = (parse_parens cols t) in
                    parse_connectors cols (intersect m1 m2, tokens_2)
  | (`Op Or)::t -> let (m2, tokens_2) = (parse_parens cols t) in
                   parse_connectors cols (union m1 m2, tokens_2)
  | (`Op Pr)::t -> (m1, tokens_1)
  | [] -> (m1, [])
  | _ -> failwith "improper operator placement"

(* convert [tokens] from lexer to the indices of matched values in [cols] *)
let parse (cols:column list) tokens : int list =
  if tokens = [] then all_indices cols else
    let (matches, leftovers) = parse_parens cols tokens in
    if leftovers = [] then matches else failwith "extra closing parentheses"