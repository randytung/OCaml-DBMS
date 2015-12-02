#load "nums.cma";;

open Big_int

let pass_length = 12
let exp_num = 6

let ( *** ) a b = int_of_float ((float_of_int a) ** (float_of_int b))

let big_str a = big_int_of_string a
let str_big a = string_of_big_int a
let go_big a = big_int_of_int a
let go_small a = int_of_big_int a
let big_zero = zero_big_int
let big_one = unit_big_int
let ( +! ) a b = add_big_int a b
let ( -! ) a b = sub_big_int a b
let ( *! ) a b = mult_big_int a b
let ( **! ) a b = power_big_int_positive_int a b
let ( /! ) a b = div_big_int a b
let ( %! ) a b = mod_big_int a b
let ( <! ) a b = lt_big_int a b
let ( >! ) a b = gt_big_int a b
let ( =! ) a b = eq_big_int a b


(* Converts string message to list of corresponding numbers. *)
let str_to_nums str =
  let rec blow_up = function
    | "" -> []
    | s -> (String.get s 0) :: blow_up (String.sub s 1 ((String.length s) - 1)) in
  let char_lst = blow_up str in
  List.map (fun x -> string_of_int (int_of_char x)) char_lst

(* Takes a list of msgs to encrypt/decrypt, along with k and m. *)
let encrypt msg k m =
  (* Uses exponentiation by squaring and modding recursively*)
  let rec encrypt' msg' k' m' leftovers =
    if k' %! (go_big 2) =! big_one
    then
      encrypt' msg' (k' -! big_one) m' ((leftovers *! msg') %! m')
    else
      if k' =! big_zero
      then
        leftovers %! m'
      else
        encrypt' ((msg' *! msg') %! m') (k' /! (go_big 2)) m' leftovers in
  List.map (fun x -> str_big (encrypt' (big_str x) k m big_one)) msg

(* Finds the totient associated with p and q. *)
let totient p q = (p -! big_one) *! (q -! big_one)

(* Finds the inverse of k with regards to m, so (k * k^-1 mod m) = 1*)
let inverse k totient =
  (* Uses Bezout's theorem: in 1 = gcd(k, m) = sk + tm, s is k's inverse. *)
  let rec bezout a b =
    if b =! big_zero
    then
      (big_one, big_zero)
    else
      let (q,r) = (a /! b, a %! b) in
      let (s',t') = bezout b r in
      (t', (s' -! (q *! t'))) in
  if k >! totient
  then
    (totient +! (fst (bezout k totient))) %! totient
  else
    (totient +! (snd (bezout totient k))) %! totient

(* Converts decrypted numbers to the corresponding string. *)
let nums_to_str nums =
  let rec build = function
    | [] -> ""
    | h::t -> (String.make 1 h) ^ build t in
  let lst = List.map (fun x -> char_of_int (int_of_string x)) nums in
  build lst

let prime_check n =
  if n =! (go_big 2) || n =! (go_big 3) then true
  else if n %! (go_big 2) =! big_zero || n %! (go_big 3) =! big_zero then false
  else
    let rec check n i w =
      if i *! i >! n then true
      else if n %! i =! big_zero then false
      else check n (i +! w) ((go_big 6) -! w) in
    check n (go_big 5) (go_big 2)

let rec next_prime n =
  if n %! (go_big 2) =! big_zero then next_prime (n +! big_one)
  else if prime_check n then n
  else next_prime (n +! big_one)

let build_p_q char_nums =
  let rec build i lst =
    match lst with
    | h::t -> big_str h *! (go_big (exp_num *** i)) +! (build (i-1) t)
    | [] -> big_zero in
  (next_prime (build pass_length char_nums), next_prime (build pass_length (List.rev char_nums)))

let build_ks char_nums totient =
  let rec check_k k t =
    let new_k = next_prime k in
    if t %! new_k =! big_zero then check_k new_k t else new_k in
  List.map (fun x -> check_k (big_str x **! exp_num) totient) char_nums

let fit_string str len =
  let diff = len - (String.length str) in
  if diff < 0 then failwith "wat"
  else if diff = 0 then str
  else String.make diff '0' ^ str

let full_encrypt pass msg =
  let pass_nums = str_to_nums pass in
  let (p,q) = build_p_q pass_nums in
  let m = p *! q in
  let tot = totient p q in
  let len = String.length (str_big tot) in
  let ks = build_ks pass_nums tot in
  let msg_nums = str_to_nums msg in
  let e_msg_nums = List.fold_left (fun x y -> encrypt x y m) msg_nums ks in
  List.fold_left (fun x y -> x ^ (fit_string y len)) "" e_msg_nums

let full_decrypt pass msg =
  let pass_nums = str_to_nums pass in
  let (p,q) = build_p_q pass_nums in
  let m = p *! q in
  let tot = totient p q in
  let len = String.length (str_big tot) in
  let ks = build_ks pass_nums tot in
  let iks = List.rev (List.map (fun x -> inverse x tot) ks) in
  let rec split str =
    let str_len = String.length str in
    if str_len <= len then [str]
    else
      (String.sub str 0 len)::(split (String.sub str len (str_len - len))) in
  let msg_nums = split msg in
  let e_msg_nums = List.fold_left (fun x y -> encrypt x y m) msg_nums iks in
  nums_to_str e_msg_nums

let stringer file =
  let ic = open_in file in
  let rec rep inchan acc =
    try rep inchan (acc ^ (input_line inchan) ^ "\n")
    with _ -> close_in_noerr ic; acc in
  rep ic ""