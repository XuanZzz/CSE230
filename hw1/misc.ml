(* CSE 130: Programming Assignment 1
 * misc.ml
 *)

(* sumList : int list -> int 
 * input: list of int
 * output: sum of all the elements in list, if empty then return 0
 * e.g. [1;2;3;4] -> 10 
 *) 
let rec sumList l = 
	match l with
	| [] 	-> 0
	| (h::t)-> h + sumList t
;;


(* digitsOfInt : int -> int list 
 * input: non-negative number, assuming no leading zeros
 * output: list of the digits of the input number
 * (see the digits function below for an example of what is expected)
 *)
let rec digitsOfInt n =
 	let rec digitsHelper n digits = 
		if n < 10
			then n::digits
			else digitsHelper (n / 10) ((n mod 10)::digits)
	in digitsHelper n []
;;

(* digits : int -> int list
 * (digits n) is the list of digits of n in the order in which they appear
 * in n
 * e.g. (digits 31243) is [3,1,2,4,3]
 *      (digits (-23422) is [2,3,4,2,2]
 *)
 
let digits n = digitsOfInt (abs n)


(* From http://mathworld.wolfram.com/AdditivePersistence.html
 * Consider the process of taking a number, adding its digits, 
 * then adding the digits of the number derived from it, etc., 
 * until the remaining number has only one digit. 
 * The number of additions required to obtain a single digit from a number n 
 * is called the additive persistence of n, and the digit obtained is called 
 * the digital root of n.
 * For example, the sequence obtained from the starting number 9876 is (9876, 30, 3), so 
 * 9876 has an additive persistence of 2 and a digital root of 3.
 *)


(* 
 * sumDigits: helper function int -> int 
 * input: a non negative int
 * output: sum of all digits in input num
 * e.g. 1234 -> 10
 *)
let rec sumDigits num = 
	sumList (digits num)
;;

(*
 * additivePersistence: int -> int
 * input: a number (positive, zero, or negative)
 * output: the times of additions to obtain a single digit from the input
 * e.g. 9876 -> 2
 *)
let rec additivePersistence n = 
	let n = abs n in 	
		if n < 10
			then 0
		else 1 + additivePersistence (sumDigits n)
;;

(*
 * digitalRoot: int -> int
 * input: a number (positive, zero, or negative)
 * output: the digit obtained from taking multiple additions of number to reduce to single digit
 * e.g. 9876 -> 3
 *)
let rec digitalRoot n = 
	if n < 10
		then n
	else digitalRoot (sumDigits n)
;;


(*
 * listReverse list -> list
 * input: a list of values (of any type)
 * output: a list that contains elements of input in reversed order
 * e.g. [1;2;3;4] -> [4;3;2;1]
 *)
let rec listReverse l = 
	let rec listReverseHelper l r = 
		match l with
		| [] -> r
		| (h::t) -> listReverseHelper t (h::r)
	in listReverseHelper l []
;;

(* explode : string -> char list 
 * (explode s) is the list of characters in the string s in the order in 
 *   which they appear
 * e.g.  (explode "Hello") is ['H';'e';'l';'l';'o']
 *)
let explode s = 
  let rec _exp i = 
    if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in
  _exp 0

(*
 * palindrome: string -> bool
 * input: any string
 * output: true if the string is a palindrome, otherwise false 
 		(palindrome: reads the same from left-to-right and right-to-left)
 * e.g. "hello" -> false
 * 		"abcba" -> true
 *		""		-> true
 *		"a"		-> true
 *)
let palindrome w = 
	let l = explode w in
		l = listReverse l
;;

(************** Add Testing Code Here ***************)
