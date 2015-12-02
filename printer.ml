open Types

let len = S.length

(* two less than actual length, to account for borders *)
let screen_len = 78
let max_num_cols = 7
let val_buffer = 8

let get_col_len num_cols = (screen_len - num_cols + 1) / num_cols

let rec repeat_str str num =
  if num = 0 then ""
  else str^(repeat_str str (num-1))

let rec num_lst len acc =
  if len = 0 then acc else num_lst (len-1) ((len-1)::acc)

let get_center_start str_length alloted_width =
  (alloted_width - str_length) / 2

let rec limit_cols num cols =
  if num = 0 && cols <> [] then ([], true) else
  match cols with
  | h::t -> let (acc, bln) = limit_cols (num-1) t in (h::acc, bln)
  | [] -> ([], false)

let build_unit str max_len =
  let s = if len str > max_len then S.sub str 0 (max_len - 3) ^ "..."
          else
            let center_start = get_center_start (len str) max_len in
            (repeat_str " " center_start) ^ str ^
            (repeat_str " " (max_len - center_start - (len str))) in
  "|" ^ s

let print tbl_name col_lst =
  let (cols,is_truncated) = limit_cols max_num_cols col_lst in
  let title = tbl_name ^ (if is_truncated then " (Truncated)" else "") in
  let num_cols = List.length cols in
  let col_len = get_col_len num_cols in
  let full_len = num_cols * (col_len + 1) - 1 in
  let tbl_bar = "+" ^ (repeat_str "-" full_len) ^ "+\n" in
  let col_bar = repeat_str ("+" ^ (repeat_str "=" col_len)) num_cols ^ "+\n" in
  let val_bar = repeat_str ("+" ^ (repeat_str "-" col_len)) num_cols ^ "+\n" in
  let pr_t = tbl_bar ^ (build_unit title full_len) ^ "|\n" ^ col_bar in
  let add_col x y =
    x ^ build_unit y.name col_len in
  let pr_t_cs = pr_t ^ (List.fold_left add_col "" cols) ^ "|\n" ^ col_bar in
  let add_i_vals acc i =
    let add_col_val x y =
      x ^ build_unit (S.escaped (val_to_string (List.nth y.vals i))) col_len in
    acc ^ (List.fold_left add_col_val "" cols) ^ "|\n" ^ val_bar in
  let i_lst = num_lst (try List.length (List.hd cols).vals with _ -> 0) [] in
  let pr_t_cs_vs = pr_t_cs ^ List.fold_left add_i_vals "" i_lst in
  print_string pr_t_cs_vs