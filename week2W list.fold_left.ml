(* Tail recursion: every recursive call is immediately followed by return *)

(* regular recursion max list *)
let rec max_list l = 
	match l with
	| []	-> 0
	| h::t	-> max h (max_list t)
;;

(* tail recursion max list, keep a max_so_for for returning *)
let max_list l = 
	let rec helper max_so_far remaining =
		match remaining with
		| []	-> max_so_far
		| h::t	-> helper (max h max_so_far) t
	in helper 0 l
;;

(* regular factorial function *)
let rec fac n = if n <= 1 then 1 else n * fac (n-1);;

(* regular recursion *)
let rec concat l =
	match l with
	| []	-> ""
	| h::t	-> h ^ (concat t);;

let concat l =
	let rec helper concat_so_far remaining =
		match remaining with
		| []	-> concat_so_far
		| h::t	-> helper (concat_so_far ^ h) t
	in helper "" l
;;


(* pattern ?

	let rec helper max_so_far remaining =
		match remaining with
		| []	-> max_so_far
		| h::t	-> helper (max h max_so_far) t

	let rec helper concat_so_far remaining =
		match remaining with
		| []	-> concat_so_far
		| h::t	-> helper (concat_so_far ^ h) t

 *)

let rec super_helper f so_far remaining = 
	match remaining with
	| []	-> so_far
	| h::t	-> super_helper f (f so_far h) t
;;


(* the super helper is called List.fold_left... 
 * f is called folding function
 * so_far is called accumulator
 * give each element to the folding function to fold it into the accumulator
 *)
let max_list l = List.fold_left max 0 l;;
let max_list = List.fold_left max 0;;
let concat_list = List.fold_left (^) "";;
let sum_list = List.fold_left (+) 0;;
let mult_list = List.fold_left ( * ) 1;; (* if use(*) would become comments.. need space *)*)


let cons x y = y::x;;
let myst l = List.fold_left cons [] l;;
let myst l = List.fold_left (fun x y -> y::x) [] l;;
(* what does myst do? *)
(* 
	myst [1;2]

	fold cons [] [1;2]

	acc = [] -> cons [] 1 -> [1]
	acc = [1] -> cons [1] 2 -> [2;1]

	myst = reverse

 *)
