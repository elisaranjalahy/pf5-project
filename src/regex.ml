open Regex_base

let rec repeat n l =
  if n > 0 then l@(repeat (n-1) l)
  else
    []
;;

let rec expr_repeat n e =
  match n with
  | 0 -> Eps
  | 1 -> e
  | _ -> Concat (e, expr_repeat (n - 1) e)
;;

let rec is_empty e =
  match e with
  | Eps -> true
  | Base _ -> false
  | Joker -> false
  | Concat (a, b) -> is_empty a && is_empty b
  | Alt (a, b) -> is_empty a && is_empty b
  | Star a -> is_empty a
;;

let rec null e =
  match e with
  | Eps -> true
  | Base _ | Joker -> false
  | Concat (a, b) -> null a && null b
  | Alt (a, b) -> null a || null b
  | Star a -> null a
;;

let rec is_finite e =
  match e with
  | Eps -> true
  | Base _ -> true
  | Joker -> true
  | Concat (a,b) -> is_finite a && is_finite b
  | Alt (a,b) -> is_finite a && is_finite b
  | Star _ -> false
;;

let product l1 l2 =
  failwith "À compléter"

let enumerate alphabet e =
  failwith "À compléter"

let rec alphabet_expr e =
  failwith "À compléter"

type answer =
  Infinite | Accept | Reject

let accept_partial e w =
  failwith "À compléter"
