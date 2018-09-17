(* Q1:
   pow n k: Raises the number n to the power of k,
            provided k is a non-negative integer.
            Raises exception Domain otherwise.

   Examples:
   pow 0  5 ==> 0
   pow 5  0 ==> 1
   pow 2  3 ==> 8
   pow 2 -3 ==> Exception: Domain

*)

let pow n k = 
  let rec pow' n k = match k with 
      0->1
    |_-> n * pow' n (k - 1) in
  if k >= 0 then pow' n k 
  else raise Domain;;
(* Q1:

  fib n : Computes the n-th element of the Fibonacci sequence:

   1, 1, 2, 3, 5, ..., fib (n-1) + fib (n -2)

   Raises exception Domain if n is negative.

  Examples:

  fib 0 => 1
  fib 10 => 89
  fib -1 => Exception: Domain

*)


let rec fib n = 
  let rec fib' n = match n with 
      0->1
    |1->1
    |n-> fib'(n-1) + fib'(n-2) in
  if n >= 0 then fib' n
  else raise Domain;;


(* Q2: Newton-Raphson method for computing the square root
*)
let square_root a =
  let rec findroot x acc =
    if ((abs_float ((x *. x) -. a)) > acc) 
    then findroot ((x +. (a /. x)) /. 2.0) acc 
    else x
  in
  if a > 0.0 then
    findroot 1.0 0.00000000000001
  else
    raise Domain


(* Q3: Tail-recursive version of power function *)

let pow_tl n k =
  let rec aux n k acc =
    match k with 
      0->acc
    |_-> (aux n (k-1) (n*acc))
  in
  aux n k 1;;

(* Q4: Checking naivly whether a given integer is a prime number *)


let is_prime n =
  let rec is_prime' n currDivisor = 
    if (currDivisor = 1) then true
    else if (n mod currDivisor = 0) then false
    else is_prime' n (currDivisor-1)
  in
  if (n <= 1) then raise Domain
  else is_prime' n (n-1);;


(* Q5: Computing the greatest common divisor using Euclid's algorithm *)

let gcd a b = 
  let rec gcd' x y =
    if (y != 0) then gcd' y (x mod y)
    else x
  in
  if (a<0) || (b<0) then raise Domain
  else gcd' a b;;
