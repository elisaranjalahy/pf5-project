type base = A | C | G | T | WC (* wildcard *)

type dna = base list



(*---------------------------------------------------------------------------*)
(*                               ECHAUFFEMENT                                *)
(*---------------------------------------------------------------------------*)


let string_of_base (b : base) : string =
  match b with
    A -> "A"
  | C -> "C"
  | G -> "G"
  | T -> "T"
  | WC -> "."


(* explode a string into a char list *)
let explode (str : string) : char list =
  let rec newCharList i list =
    if i < 0 then list      
    else newCharList (i - 1) (str.[i] :: list)
    in
    newCharList (String.length str - 1) []
  ;;


(* conversions *)
let base_of_char (c : char) : base =
  match c with
    | 'A' -> A
    | 'T' -> T
    | 'G' -> G
    | 'C' -> C
    | _ -> WC
;;


let dna_of_string (s : string) : base list =
  let rec newBaseList i list = 
    if i < 0 then list
    else newBaseList (i-1) ((base_of_char s.[i])::list)
  in
  newBaseList (String.length s -1) []
;;



let string_of_dna (seq : dna) : string =
  let rec aux seq =
    match seq with
    | [] -> ""
    |[b]->string_of_base b
    | bb :: seq ->(string_of_base bb)^(aux seq) 
  in
  aux seq
;;




(*---------------------------------------------------------------------------*)
(*                                   SLICES                                  *)
(*---------------------------------------------------------------------------*)
(*
   Une {\em tranche} de $l = \langle x_1,\dots x_n\rangle$ est une sous-liste
   de $l$ de la forme $\langle x_i, \dots x_{j}$, o\`u $1 \leq i \leq j\leq n$.
 *)


(* if list = pre@suf, return Some suf. otherwise, return None *)
let rec cut_prefix (slice : 'a list) (list : 'a list) : 'a list option =
  if List.length slice > List.length list then None 
  else
  match (slice, list) with
  | ([], suf) -> Some suf
  | (x::slice_tail, y::list_tail) -> if x = y then cut_prefix slice_tail list_tail
    else 
      None 
  |(_::_,[])->None
;;


(*
  cut_prefix [1; 2; 3] [1; 2; 3; 4] = Some [4]
  cut_prefix [1; 2; 3; 4] [1; 2; 3; 4] = Some []
  cut_prefix [1; 2; 0] [1; 2; 3; 4] = None
 *)


(* return the prefix and the suffix of the first occurrence of a slice,
   or None if this occurrence does not exist.
*)
let first_occ (slice : 'a list) (list : 'a list)
    : ('a list * 'a list) option =
    let rec get_before_list slice list newList =
      match slice, list with 
      | [],_ -> Some ([],list)
      | _,[] -> None
      | x::slice_tail, y::list_tail -> 
        match cut_prefix slice list with
        |None -> get_before_list slice list_tail (y::newList)
        |Some(suf) -> Some(List.rev newList,suf)
      in get_before_list slice list []
;;

(*
  first_occ [1; 2] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([1; 1], [3; 4; 1; 2])
  first_occ [1; 1] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([], [1; 2; 3; 4; 1; 2])
  first_occ [1; 3] [1; 1; 1; 2; 3; 4; 1; 2] = None
 *)


let rec slices_between
          (start : 'a list) (stop : 'a list) (list : 'a list) : 'a list list =
  match first_occ start list with
  | None -> []
  | Some (before_start, after_start) -> (
    match first_occ stop after_start with
      | None -> []
      | Some (before_stop, after_stop) ->
        [before_stop] @ slices_between start stop after_stop
  )
;;

  

(*
  slices_between [1; 1] [1; 2] [1; 1; 1; 1; 2; 1; 3; 1; 2] = [[1]]
 *)

let cut_genes (dna : dna) : (dna list) = 
  slices_between [A; T; G] [T; A; A] dna
;;


(*---------------------------------------------------------------------------*)
(*                          CONSENSUS SEQUENCES                              *)
(*---------------------------------------------------------------------------*)


type 'a consensus = Full of 'a | Partial of 'a * int | No_consensus

(* return (Full a) if all elements of the list are equal to a,
   (Partial (a, n)) if a is the only element of the list with the
   greatest number of occurrences and this number is equal to n,
   No_consensus otherwise. *)
let rec nbr_occ l b: int =
  match l with
  |[] -> 0
  |[a]-> if a==b then 1 else 0
  |x::ll -> if x==b then 1+nbr_occ ll b else nbr_occ ll b
  
;;
  
let rec enlever_doublons  (list : 'a list) : 'a list =
  match list with
    | [] -> []
    | hd :: tl ->
        let sans_hd = List.filter (fun x -> x <> hd) tl in
        hd :: enlever_doublons sans_hd
;;

let occ lst : ('a * int) list =
  let e = enlever_doublons lst in
  List.map (fun x -> (x, nbr_occ lst x)) e
;;



let consensus (list : 'a list) : 'a consensus =
  let occurrences = occ list in
    let max_occ= ref 0 in
    let consensus_valeur = ref None in

    List.iter (fun (valeur, nbr) ->
      if nbr > !max_occ then (
        max_occ := nbr;
        consensus_valeur := Some valeur
      ) else if nbr = !max_occ then (
        consensus_valeur := None
      )
    ) occurrences;

    match !consensus_valeur with
    | Some valeur when !max_occ = List.length list -> Full valeur
    | Some valeur -> Partial (valeur, !max_occ)
    | None -> No_consensus
;;

(*
   consensus [1; 1; 1; 1] = Full 1
   consensus [1; 1; 1; 2] = Partial (1, 3)
   consensus [1; 1; 2; 2] = No_consensus
 *)

(* return the consensus sequence of a list of sequences : for each position
   in the elements of ll, compute the consensus  of the set of values at this
   position  in the sequences. the lists must be of same length. if all lists
   are empty, return the empty sequence.
 *)

 let consensus_sequence (ll : 'a list list) : 'a consensus list =
  let taille = match ll with 
  |[] -> 0 
  | hd :: _ -> List.length hd in
  List.init taille (fun pos ->
    let valeur = List.map (fun l -> List.nth l pos) ll in
    consensus valeur
  )
;;

(*
 consensus_sequence [[1; 1; 1; 1];
                     [1; 1; 1; 2];
                     [1; 1; 2; 2];
                     [1; 2; 2; 2]]
 = [Full 1; Partial (1, 3); No_consensus; Partial (2, 3)]

 consensus_sequence [[]; []; []] = []
 *)
