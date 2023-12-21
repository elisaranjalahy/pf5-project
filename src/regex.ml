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
  | Base _ -> false
  | Joker -> false
  | Concat (a, b) -> null a && null b
  | Alt (a, b) -> null a || null b
  | Star _ -> true 
;;



let rec is_finite_aux e=
  match e with
  | Eps->true
  | Base a->false
  | Joker -> false
  | Concat(Eps,Eps)->true
  | Alt(Eps,Eps)-> true
  | Star b-> is_finite_aux b
  | Concat(a,b)->false
  | Alt(a,b) -> false
;;

let rec is_finite e =
  match e with
  | Eps -> true
  | Base _ -> true
  | Joker -> true
  | Concat (a,b) -> is_finite a && is_finite b
  | Alt (a,b) -> is_finite a && is_finite b
  | Star a -> is_finite_aux a
;;

let product l1 l2 =
  let concatener hd1 a = hd1 @ a in
  let rec product_aux acc l1 =
    match l1 with
    | [] -> acc
    | x :: ll1 ->
        let acc' = union_sorted acc (List.map (concatener x) l2) in
        product_aux acc' ll1
  in
  product_aux [] l1
;;


let rec enumerate alphabet e =
  match e with
  | Eps -> Some [[]]
  | Base a -> Some [[a]]
  | Joker -> Some (List.map (fun a -> [a]) alphabet)
  | Concat (a, b) -> 
      (match (enumerate alphabet a), (enumerate alphabet b) with
       | Some p, Some q -> Some (List.concat (List.map (fun a -> List.map (fun b -> a@b)q)p))
       | _, _ -> None)
  | Alt (a,b) -> 
      (match (enumerate alphabet a), (enumerate alphabet b) with
       | Some p, Some q -> Some (p@q)
       | _, _ -> None)
  | Star a -> if (is_empty a) then Some [[]] else None

;;

let rec alphabet_expr e =
  match e with
  | Eps -> []
  | Base a -> sort_uniq [a]
  | Joker -> [] 
  | Concat (a, b) -> sort_uniq (alphabet_expr a @ alphabet_expr b)
  | Alt (a, b) -> sort_uniq (alphabet_expr a @ alphabet_expr b)
  | Star a -> sort_uniq (alphabet_expr a)
;;

type answer =
  Infinite | Accept | Reject


let accept_partial e w =
  failwith "à completer"

;;

  
