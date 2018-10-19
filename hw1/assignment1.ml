(* 1(a) *)
let rec sumList l =
	match l with
	| [] 	-> 0
	| (h::t)-> h + sumList t;;

(* 1(b) *)
let rec digitsOfInt num = 
	let rec digitsHelper num digits = 
		if num <= 0
			then digits
			else digitsHelper (num / 10) ((num mod 10)::digits)
	in digitsHelper num []
;;

(* 1(c) *)
let rec sumDigits num = 
	if num <= 10
		then num
	else (num mod 10) + sumDigits(num / 10)
;;

let rec additivePersistence num = 
	if num < 10
		then 0
	else 1 + additivePersistence (sumDigits num);;

let rec digitalRoot num =
	if num < 10
		then num
	else digitalRoot (sumDigits num);;


(* 2(a) *)
let rec listReverse l =
	let rec listReverseHelper l r = 
		match l with
		| [] -> r
		| (h::t) -> listReverseHelper t (h::r)
	in listReverseHelper l [];;


let rec listReverse l = 
	match l with
	| [] -> []
	| (h::t) -> (listReverse t)@[h];;


(* 2(b) *)
let explode s = 
  let rec _exp i = 
    if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in
  _exp 0
;;


let palindrome w = 
	let l = explode w in
		l = listReverse l
;;

