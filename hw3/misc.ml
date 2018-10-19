(* CSE 130: Programming Assignment 3
 * misc.ml
 *)

(* For this assignment, you may use the following library functions:

   List.map
   List.fold_left
   List.fold_right
   List.split
   List.combine
   List.length
   List.append
   List.rev

   See http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html for
   documentation.
*)



(* Do not change the skeleton code! The point of this assignment is to figure
 * out how the functions can be written this way (using fold). You may only
 * replace the   failwith "to be implemented"   part. *)



(*****************************************************************)
(******************* 1. Warm Up   ********************************)
(*****************************************************************)

let sqsum xs = 
  let f a x = a + x * x in
  let base = 0 in
    List.fold_left f base xs

let pipe fs = 
  let f a x = (fun y -> x (a y)) in
  let base = (fun x -> x) in
    List.fold_left f base fs

let rec sepConcat sep sl = match sl with 
  | [] -> ""
  | h :: t -> 
      let f a x = a ^ sep ^ x in
      let base = h in
      let l = t in
        List.fold_left f base l

(* tostring([f val for val in l]) *)
let stringOfList f l = "[" ^ (sepConcat "; " (List.map f l)) ^ "]"

(*****************************************************************)
(******************* 2. Big Numbers ******************************)
(*****************************************************************)

(* input: element x and times to repeat n
 * output: list of n elements of x
 * e.g. clone 3 5 -> [3;3;3;3;3]
 *)
let rec clone x n = 
  if n <= 0 then []
  else x::(clone x (n-1))
;;

(* input: two lists l1 l2
 * output: two lists of equal length (shorter input list gets leading zeros)
 * e.g. [1;2;3], [4;5] -> ([1;2;3], [0;4;5])
 *)
let rec padZero l1 l2 = 
  let len1 = List.length l1 in
  let len2 = List.length l2 in
  if len1 <= len2 
    then ((clone 0 (len2 - len1))@l1, l2)
    else (l1, (clone 0 (len1 - len2))@l2)

(* input: list of int
 * output: input without leading zero elements
 * e.g. [0;0;2;0;3] -> [2;0;3]
 *)
let rec removeZero l = 
  match l with
  | []    -> []
  | h::t  -> if h = 0 then (removeZero t) else l

(* input: two lists of integers
 * output: sum of integers represented by the two input lists
 * e.g. [9;9] [1;0;0;2] -> [1;1;0;1] 
 *)
let bigAdd l1 l2 = 
  let add (l1, l2) = 
    let f a x = let (carry, curr_res) = a in 
                let (d1,d2) = x in
                let curr_sum = carry + d1 + d2 in
                if curr_sum > 9 then (1, (curr_sum mod 10)::curr_res)
                else (0, (curr_sum)::curr_res)
        in
    let base = (0, []) in
    let args = (List.rev (List.combine l1 l2))@[(0,0)] in
    let (_, res) = List.fold_left f base args in
      res
  in 
    removeZero (add (padZero l1 l2))

(* input: digit i, int represented list l
 * output: input multiplied by digit i
 * e.g. 9 [9;9;9;9] -> [8;9;9;9;1]
 *)
let rec mulByDigit i l = 
  let fold_fn acc elmt = 
    let (carry, curr_res) = acc in
    let curr_mut = elmt * i + carry in
    (curr_mut / 10, (curr_mut mod 10)::curr_res) in
  let base = (0, []) in
  let (_, res) = List.fold_left fold_fn base ((List.rev l)@[0]) in
  removeZero res

(* input: two lists of integers
 * output: product of integers represented by the two input lists
 * e.g. [9;9;9;9] [9;9;9;9] -> [9;9;9;8;0;0;0;1] 
 *)
let bigMul l1 l2 = 
  let f a x = 
    let (wei, curr_res) = a in
    (wei+1, (bigAdd ((mulByDigit x l2)@(clone 0 wei)) curr_res)) 
  in
  let base = (0, []) in
  let args = (List.rev l1)@[0] in
  let (_, res) = List.fold_left f base args in
    res
