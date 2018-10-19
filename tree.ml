type tree = Leaf of int | Node of tree * tree;;

let rec sum_leaves t =
	match t with
	| Leaf i	-> i
	| Node (l,r)-> sum_leaves l + sum_leaves r
;;

(* double the value of each leaf *)
(* this copies and returns a new tree *)
let rec double_leaves t = 
	match t with
	| Leaf i	-> Leaf (2 * i)
	| Node (l,r)-> Node (double_leaves l, double_leaves r)
;;


let rec create_tree height max =
	if height <= 0 then Leaf (Random.int max)
else Node(create_tree (height - 1) max, create_tree (height-1) max);;

(* define a type to represent arithmetic expression *)
type expr = 
	| Const of int
	| Sum of (int * int);;

(* this won't work, cuz Sum(10, 10) is type expr not type int *)
Sum(10, Sum(10, 10));;

(* can do this: change to expr, expr *)
type expr = 
	| Const of int
	| Sum of (expr * expr)
	| Exp of (expr * expr)
;;

Sum(Const 10, Sum((Const 10), (Const 10)));;


(* since ** needs float, need to use float 
 * Const of float, + -> +. 
 *)
let rec eval e = 
	match e with 
	| Const i		-> i
	| Sum (l, r)	-> eval l + eval r
	| Exp (a, b)	-> (eval a) ** (eval b)
;;


(* tail recursion *)
let rec length l =
	match l with
	| []	-> 0
	| h::t	-> 1 + length t
;;

let tr_len l =
	let helper remaining_elmts result =
		match remaining_elmts with
		| []	-> result
		| h::t	-> helper t (1 + result)
	(* here call the helper function *)
	in helper l 0
;;
