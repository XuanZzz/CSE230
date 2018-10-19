(* a function that applie f to each element of l *)
let rec map f l =
	match l with
	| []	-> []
	| h::t	-> (f h)::(map f t)
;;

map (fun x -> x*x) [1;2;3];;

let incr_list l = map (fun x -> x+1) l;;
let incr_list = map ((+) 1);;

(* a list of int -> int functions *)
let incr_list = map (+);;
(* now map again, map to take 10 as input
	output would be [11;12;13]
 *)
map (fun f -> f 10) (incr_list [1;2;3]);;

(* how to make it tail recursion, using fold_left as super helper *)
let map f l = 
	let fold_fn acc elmt = acc@[f elmt] in 
	let acc = [] in (* return should be list, so acc should start as [] *)
	List.fold_left fold_fn acc l
;;
