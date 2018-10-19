let max x y = if x > y then x else y;;

(* if contains negative elements return 0 for [] won't work *)
let rec list_max xs = 
	match xs with
	| []	-> 0
	| x::t  -> max x (list_max t)
;;

(* doesnt work with empty list *)
let rec list_max xs = 
	match xs with
	| [x]	-> x
	| x::t  -> max x (list_max t)
;;


(* another way to handle the empty list *)
type int_maybe =
	| None
	| Some of int
;;

(* now let it return a int_maybe *)
let rec max_list l = 
	match l with
	| []	-> None
	| h::t	-> match (max_list t) with
				| None		-> Some h
				(* t_max is an int, can call with max *)
				| Some t_max-> Some (max h t_max)
;;


(* filter: keeps what in xs that satisfies f *)
let rec filter f xs =
	match xs with
	| []	-> []
	| x::t	-> if f x then x::(filter f t) else filter f t
;;

(* only wants to call filter f t once ? *)
let rec filter f xs =
	match xs with
	| []	-> []
	| x::t	-> (if f x then [x] else [])@(filter f t)
;;

