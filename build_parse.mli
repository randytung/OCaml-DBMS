open Types

(* returns a pair [(word, rest)] where [word] is the input string up to but
 * not including the first instance of del.
 * [rest] is the rest of the input string, with the first delimiter removed.
 * [rest] is an option, because if [input] had no dels, the whole
 * string is returned in [word] and None is returned in [rest].*)
val chr_split : string -> char -> string * string option

(* returns a list of chunks that make up [input] delimited by [del].
 * if [trim], each element of the list is trimmed to remove extra spaces. *)
val chr_split_lst : string -> char -> bool -> string list

(* returns an option - None if [match_str] isn't present in the string or is "".
 * otherwise, returns [Some (before, after)] where [before] is the
 * string before the match word, and [after] is the string after the match word.
 * matches are blind to case. *)
val str_split : string -> string -> string option

(* returns the next word of a str (split by spaces) as fst, then for snd either
 * the the rest of the string or the empty string, both of which are trimmed *)
val next_word : string -> string * string

(* removes a redundant word from a string before performing function [f].
 * fails if the redundant word wasn't what was expected. *)
val extra_word : string -> (string -> 'a) -> string -> 'a

val trim_parens : string -> string

val check_format : string -> bool