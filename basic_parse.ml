open Types

(* returns a pair [(word, rest)] where [word] is the input string up to but
 * not including the first instance of del.
 * [rest] is the rest of the input string, with the first delimiter removed.
 * [rest] is an option, because if [input] had no dels, the whole
 * string is returned in [word] and None is returned in [rest].*)
let chr_split str del =
  let loc = try Some (S.index (S.lowercase str) del) with _ -> None in
  match loc with
  | None   -> None
  | Some i -> let first = S.sub str 0 i in
              let second = S.sub str (i+1) ((S.length str) - (i+1)) in
              Some (first, second)

(* returns a list of chunks that make up [input] delimited by [del].
 * if [trim], each element of the list is trimmed to remove extra spaces. *)
let rec chr_split_lst input del trim =
  let (h,t) = match chr_split input del with
              | None -> (input, None)
              | Some (a,b) -> (a, Some b) in
  let h' = if trim then S.trim h else h in
  match t with
  | None -> [h']
  | Some s -> h'::(chr_split_lst s del trim)

(* returns an option - None if [match_str] isn't present in the string or is "".
 * otherwise, returns [Some (before, after)] where [before] is the
 * string before the match word, and [after] is the string after the match word.
 * matches are blind to case. *)
let str_split str del =
  let rec s_s d s acc =
    let (d', s') = (S.lowercase d, S.lowercase s) in
    let (l_m, l_s) = (S.length d, S.length s) in
    if not (S.contains s' d'.[0]) then None
    else
      let i = S.index s' (d'.[0]) in
      if l_s - i < l_m then None
      else
        let sub = S.sub s' i l_m in
        if sub = d' then Some (acc^(S.sub s 0 i),
                               S.sub s (i+l_m) (l_s - l_m - i))
        else s_s d (S.sub s (i+1) (l_s - i - 1)) (acc^(S.sub s 0 (i+1))) in
  if del = "" then None
  else if S.length del = 1 then chr_split str (S.lowercase del).[0]
  else s_s del str ""

(* returns the next word of a str (split by spaces) as fst, then for snd either
 * the the rest of the string or the empty string, both of which are trimmed *)
let next_word s =
  match chr_split (S.trim s) ' ' with
  | Some (a,b) -> (S.trim a, S.trim b)
  | None       -> (s, "")

(* removes a redundant word from a string before performing function [f].
 * fails if the redundant word wasn't what was expected. *)
let extra_word word f str =
  let (test, rest) = next_word str in
  if word = S.lowercase test then f rest
  else failwith ("missing keyword '" ^ word ^ "'")

let trim_parens str =
  let len = S.length str in
  if str.[0] <> '(' || str.[len-1] <> ')'
  then failwith "missing outer parentheses"
  else S.sub str 1 (len-2)

let reserved_words =
  ["select"; "where"; "and"; "or"; "insert"; "into"; "update";
   "delete"; "like"; "in"; "between"; "values"; "from"; "table";
   "null"; "string"; "integer"; "float"; "bool"; "drop"; "alter";
   "add"; "modify"; "column"; "value"]

let check_format str =
  let rec f str start =
    let len = S.length str in
    if len = 0 then not start
    else
      let char_num = int_of_char str.[0] in
      if (char_num >= 65 && char_num <= 90) ||
         (char_num >= 97 && char_num <= 122) ||
         (not start && char_num >= 48 && char_num <= 57)
      then f (S.sub str 1 (S.length str - 1)) false
      else false in
  f str true && List.for_all (fun x -> (S.lowercase str) <> x) reserved_words