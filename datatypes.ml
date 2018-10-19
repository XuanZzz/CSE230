(* this can be either one in below list *)
type attrib = 
	| Name		of string
	| Age		of int
	| DOB		of int * int * int
	| Address	of string
	| Height	of float
	| Alive		of bool
	| Email		of string
;;

(* the type of a1 would be "attrib" *)
(* these three variables have the same type attrib *)
let a1 = Name "Bob";;
let a2 = Height 5.83;;
let a3 = Age 203;;

(* then you can have a list of these attributes *)
let a_l = [a1;a2;a3];;

(* how to extract from these attrib ? -> pattern matching *)
(* let a1 = Name "Bob";; would return 0 *)
(* let a1 = Age 11;; would return 11 *)
match a1 with
	| Name	s -> 0
	| Age	i -> i
	| _		  -> 10;;

Printf.printf "1234";;

(* would only match the first one, when both Age i and i < 10 match *)
match (Age 10) with
	| Age i when i < 10 -> Printf.sprintf "%d (young)" i
	| Age i -> Printf.sprintf "%d (older)" i
	| Email s -> Printf.sprintf "%s" s
	| _ -> ""
;;


(* equivalent to if then *)
match cond with
	| true -> xxx
	| false -> xxx
;;


(* type that's recursive... *)
(* like a linked list, S means successor *)
type nat = 
	| Z
	| S of nat;;

Z;;
S Z;;
S (S Z);;


