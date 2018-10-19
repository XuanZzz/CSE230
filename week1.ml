(* pattern matching *)

(* not exhaustive warning *)
let h::t = [1;2;3;4];;

(* exception of none *)
let h::t = [];;


(* match expression: below would eval to false *)
let l = [1;2;3];;
match l with
	| [] -> true
	| h::t -> false;;

(* try tuple, would eval to (false, true) *)
((match l with
	| [] -> true
	| h::t -> false), true);;

(* would have warning, eval to 1 *)
match l with
	| [] -> 0
	| h::t -> h;;


(* function expression *)
fun x -> x+1;;

(* below two are the same (syntax sugar) *)
let inc = fun x -> x+1;;
let inc x = x + 1;
inc 0;;


let double = fun x -> x * 2;;
(* this is 6 not 8, parsed as (double 2)+2 *)
double 2+2;;


(* a function that checks if l is empty *)
let is_empty l = 
	match l with
		| [] -> true
		| _::_ -> false;; (* don't care about head/tail: put underscore *)
(* or   | _ ;;  *)


(* multiple params: no *)
let plus = fun x -> (fun y -> x+y);;

(* this would return a function that adds another number with 5 *)
plus 5;;

let f = plus 5;;
(* this evals to 15 *)
f 10;;


(* you can do: (syntax sugar) *)
let plus x y = x+y;;
(* this returns a function: no such thing as multiple params *)
g = plus 30;;


(* this function takes a pair as input *)
let plus = fun p ->
		let (x,y) = p in x+y;;

(* then this wont work cuz it needs a pair *)
plus 5;;
(* right way to use it *)
plus (4,5);;

(* less than function *)
let lt = fun x -> (fun y -> x < y);;
let is5lt = lt 5;;
(* returns true *)
is5lt 10;;


(* f should be a function that returns true or false *)
(* then this funcion negates the result *)
let neg = fun f -> fun x -> not (f x);;

(* should be false *)
let mystery = neg is5lt;;
mystery 10;;

let odd = fun x -> x mode 2 = 1;;
let even = neg odd;
