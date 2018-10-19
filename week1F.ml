(* recursive types *)
(* this Z has nothing in it, just means it's Z *)
(* S: Sessor *)
type nat = Z | S of nat;;

(* values of nat: One nat contains another *)
(* Z / S of Z / S of S *)
Z;;
S Z;;
S S Z;; (* Wrong, would be parsed as (S S) Z *)
S (S Z);;

(* operators: +*/ have type int -> int -> int *)
(* takes a nat then returns the int value of the nat *)
let rec to_int n = 
	match n with 
		| Z-> 0
		| S(n') -> 1 + to_int n' (* cannot directly use 1 + n', cuz n' is nat, not int *)
;;

(* takes an int and turns it to a nat *)
let rec to_nat i = 
	if i = 0
	then Z
	else S (to_nat (i-1))
;;

(* plus for two nat *)
(* easiest way *)
let rec plus n m = 
	to_nat((to_int m) + (to_int n));;

(* better way *)
let rec plus n m = 
	match n with
	| Z		-> m
	| S(n')	-> S (plus n' m)
;; 

(* a bit better *)
let rec plus n m = 
	match (n, m) with
	| (Z, _)		-> m
	| (_, Z)		-> n
	| (S n', S m')	-> S (S (plus n' m'))
;; 


(* multiply: just additions *)
let rec mul n m =
	match n with
	| Z 	-> Z 
	| S(n') -> plus m (mul n' m)
;;


(* better edition *)
let rec plus n m = 
	match (n, m) with
	| (Z, _)		-> Z
	| (_, Z)		-> Z
	| (_, S m')	-> plus n (mul n m')
;; 

(* very inefficient int multiplication... can be used to test our mul *)
let int_mul n m = to_int (mul (to_nat n) (to_nat m));;


(* Lists are recursive types!! *)
type int_list = 
	| Nil
	| Cons of (int * int_list) 
;;

let l = Cons (10, Cons (10, Cons (10, Nil)));;


(* use int instead: Z-> 0, S -> 1 + *)
let rec length l =
	match l with
	| Nil		-> Z
	| Cons(_, t)-> S (length t)
;;

(* 0 is good to put for Nil since it's add on recursions *)
let rec sum l =
	match l with
	| Nil			-> 0
	| Cons(h, t)	-> h + sum t
;;






