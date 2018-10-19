(* first param f: a function that returns a bool *)
(* second param l: a list *)
(* this filter would return a list that takes what's left in l that satisfies f *)
let rec filter f l =
	match l with
	| [] 	-> []
	| (h::t)->
		let t' = (filter f t) in
		if f h then h::t' else t';;

(* this would output [1;3;5] *)
filter (fun x -> x mod 2 = 1) [1;2;3;4;5;6];;

let neg f x = not (f x);;

(* this would return ([1;3;5;7], [2;4;6]) *)
let partition f l = (filter f l, filter (neg f l));;
let odd x = x mod 2 = 1;;
partition odd [1;2;3;4;5;6;7];;


(+);;

(* increment function *)
let inc = (+) 1;;

(* compare function *)
let myst = (>) 6;;
(* this would be true *)
myst 3;;


(* quicksort *)
(* use @ for combining two lists *)
let rec sort l = 
	match l with
	| []	-> []
	| (h::t)-> let (l,r) = partition ((>) h) t in
				(sort l)@(h::(sort r));;



