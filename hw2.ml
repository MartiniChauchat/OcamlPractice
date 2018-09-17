(* --------------------------------------------------------------------*)
(* QUESTION 1: House of Cards                                          *)
(* --------------------------------------------------------------------*)

(* Q1: Comparing cards *)
(* Comparing two ranks *)
let dom_rank (r1 : rank) (r2 : rank) = 
  let dom_rank' r1 r2 = 
    let k1 = match r1 with
      |Six-> 6
      |Seven-> 7 
      |Eight -> 8
      |Nine -> 9
      |Ten -> 10
      |Jack -> 11 | Queen -> 12 | King -> 13 | Ace -> 14 in 
    let k2 = match r2 with 
      |Six-> 6
      |Seven-> 7 
      |Eight -> 8
      |Nine -> 9
      |Ten -> 10
      |Jack -> 11 | Queen -> 12 | King -> 13 | Ace -> 14 in 
    if k1 >= k2 then true
    else false
  in
  dom_rank' r1 r2;;

(* Comparing two cards (r1, s1) and (r2, s2) *)
let dom_card ((s1 : rank) , (s2 : suit)) ((s3 : rank) , (s4 : suit)) = 
  let dom_card' s2 s4 = 
    let v1 = match s2 with 
      |Spades -> 4
      |Hearts -> 3
      |Diamonds -> 2
      |Clubs -> 1
    in 
    let v2 = match s4 with 
      |Spades -> 4
      |Hearts -> 3
      |Diamonds -> 2
      |Clubs -> 1
    in 
    if v1 > v2 then true
    else if v1 < v2 then false
    else dom_rank s1 s3
  in
  dom_card' s2 s4;;

(* Q2: Insertion Sort – Sorting cards in a hand *)
let rec insert (c : card) (h : hand) = match h with 
  | Empty -> Hand (c , Empty)
  | Hand(x , xs) -> if (dom_card c x) = true then Hand(c , Hand(x , xs)) 
      else Hand(x , (insert c xs));;
    
let rec sort (h : hand) : hand =
  match h with
  | Empty -> Empty
  | Hand(y , ys) -> insert y (sort ys);;

(* Q3: Generating a deck of cards *)
let rec g' (sui  : suit) (ranks:rank list)  = 
 	match ranks with 
  | [] -> []
  | h::t -> (h , sui):: (g' sui t);;

let rec generate_deck (suits : suit list) (ranks : rank list) =
  match suits with 
  | [] -> []
  | h::t -> (g' h ranks) @ (generate_deck t ranks);;



(* Q4: Shuffling a deck of cards *)
let rec split (deck : card list) (n : int) acc =
  match deck with
  |[] -> ((Six , Spades) , deck)
  |h::t -> if (n != 0) then (split t (n-1) (acc @ [h])) else (h , acc @ t);;

let shuffle (deck : card list) = 
  let size = List.length deck in
  let rec select deck n =
    if n = 0 then [] else
      match (split deck (Random.int n) []) with
      |(h, t) -> h::(select t (n-1))
  in
  select deck size

(* --------------------------------------------------------------------*)
(* QUESTION 2: Sparse Representation of Binary Numbers                 *)
(* ------------------------------------------------------------------- *)

(* Q1: Incrementing a sparse binary number *)
let pow n k =
  let rec aux n k acc =
    match k with 
      0->acc
    |_-> (aux n (k-1) (n*acc))
  in
  aux n k 1;;


let checklastsp n = 
  let rec checklastsp' x n =
    if (pow 2 x) > n then (pow 2 x)/2 else (checklastsp' (x+1) n) in
		checklastsp' 1 n;;

let convert d = 
  let rec cov acc d = 
    let d1 = (checklastsp d) in
    if d = 1 then d::acc
    else if d = d1 then d::acc
    else cov (d1::acc) (d mod d1) in 
  cov [] d

let rec spToint l=
  match l with
    []->0
  |h::t-> h+ (spToint t);;

let inc (ws : nat ) : nat =
  let i1 = ((spToint ws) + 1) in
  let ws2 = convert i1 in
  ws2

(* Q2: Decrementing a sparse binary number *)
let dec (ws : nat) : nat =
  if ws = [0] then raise Domain
  else let i1 = ((spToint ws) - 1) in
    let ws2 = convert i1 in 
    ws2

(* Q3: Adding sparse binary numbers *)
let rec add (m : nat) (n : nat) : nat  =
  let i = ((spToint m) + (spToint n)) in
  let ws = convert i in
  ws;;

  
(* Q4: Converting to integer - tail recursively *)
let rec toInt n acc =
  match n with 
  | []->acc
  | h::t -> (toInt t (acc + h));;

let rec sbinToInt l=
  match l with
    []->0
  |h::t-> h+ (sbinToInt t);;
