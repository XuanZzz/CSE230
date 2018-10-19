(*  *)
let f x =
	let y = 10 in
	let g z = x + y + z in
	g;;

(* this would return a funcion that adss 10 + 10 to input *)
let h = f 10;;
(* would output 26 *)
h 6;;

(* this time would output 36 *)
let h = f 20;;
(* so h is not just a pointer to some code of f *)
h 6;;

(* this h and h2 has two different f *)
(* so everytime we call f -> create a new f rather than using the original one *)
let h2 = f 30;;
h2 6;;

(* more examples *)
let x = 1;; (* x: 1 *)
let f y = x + y;; (* f closure: <c, ptr to the environment it's defined(-> x:1)> *)
let x = 2;; (* x: 2 *)
let y = 2;; (* y: 3 *)
f (x + y);; 
(* now go backwards, get y is 3, x is 2 so we get f 5, then evaluates f using the closure 
 * x+y -> y(in f's code): 5 -> <c, ptr->x:1>, then x is 1 + y is 5: we get 6
 *)



 (* example 2 *)
let x = 1;;
let f y = (* link to environment of <x:1> *)
	let x = 2 in
	fun z -> x + y + z
;;

let x = 100;
(* produce a function that adds 2 + 4 to input z *)
let g = (f 4);; (* <y(of f) is 4> links to f<c, ptr to <x:1>> *)
let y = 100;
(* should be 7 *)
(g 1);;


type tree = 
| Leaf of int
| Node of tree * tree
;;

(* delete all occurances of i by replacing them with 0 *)
let rec delete t i =
	match t with
	| Leaf i' -> Leaf (if i' = i then 0 else i')
	| Node (l, r) -> Node (delete l i, delete r i)
;;

let x = Node(Node(Leaf 1, Leaf 2), (Leaf 3));;
delete x 2;; (* would be 1 0, 3*)

(* if adds another Empty in tree *)
type tree = 
| Leaf of int
| Node of tree * tree
| Empty
;;
(* then would need to add this new case Empty in every function 
 * this is too violent
 *)
let rec delete t i =
	match t with
	| Leaf i' -> Leaf (if i' = i then 0 else i')
	| Node (l, r) -> Node (delete l i, delete r i)
	| Empty -> Empty
;;

