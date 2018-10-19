(* implement partition using fold *)
let partition f l = 
	let fold_fn (left, right) elmt = 
		if (f elmt) 
		then (left@[elmt], right) 
		else (left, right@[elmt]) 
		in
	let base = ([], []) in
	List.fold_left fold_fn base l
;;

(* implement partition using fold *)
let partition f l = 
	let fold_fn (left, right) elmt = 
	(* if f takes a long time dont call it twice use let x = f elmt*)
		(left@(if (f elmt) then [elmt] else []),
		 right@(if (f elmt) then [] else [elmt])) in
	let base = ([], []) in
	List.fold_left fold_fn base l
;; 


(* function composition *)
let compose f g = fun x -> f (g x);;
let compose f g x = f (g x);;

(* add 1 to every elmt in list *)
let f = List.map ((+) 1);;
(* compose itself... *)
let f = compose f f;;
(* would output [3;4;5] *)
f [1;2;3];;


let x = 10;
(* this works.. *)
let f y = x + y;;
let x = 20;;
(* this still outputs 15 *)
f 5;;


let f x = 
	let z = 30 in
	let g y = x + y + z in g;;

let myst = f 10;;
(* this is 10 + 30 + 50 = 90 *)
myst 50;;
