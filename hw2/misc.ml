(* CSE 130: Programming Assignment 2
 * misc.ml
 *)

(* ***** DOCUMENT ALL FUNCTIONS YOU WRITE OR COMPLETE ***** *)
(* input: (default return value, key to search in list, list of (key,value))
 * output: the first value with the matched key in list, default if key not found
 *)
let rec assoc (d,k,l) = 
    match l with
    | []    -> d
    | h::t  -> let (x,y) = h in
                if x = k then y
                else assoc (d, k, t)
;;


(* fill in the code wherever it says : failwith "to be written" *)
(* input: list of int 
 * output: list that removes duplicate elements within input
 *)
let removeDuplicates l = 
  let rec helper (seen,rest) = 
      match rest with 
        [] -> seen
      | h::t -> 
        let seen' = if(List.mem h seen) then seen else h::seen in
        let rest' = t in 
	  helper (seen',rest') 
  in
      List.rev (helper ([],l))
;;

(* Small hint: see how ffor is implemented below *)
(* input: function f: takes input b, produces b' and a boolean c'
          initial variable b
 * f b -> (b', c') while c' is true, update b with b'
 * output: the first value that makes c' evals to false, return b'
 *)
let rec wwhile (f,b) = 
  let (b', c') = f b in
  if c' then wwhile (f, b')
  else b'
;;

(* fill in the code wherever it says : failwith "to be written" *)
(* input: function f, init value b
 * output: fixpoint of f (f b = b)
 *)
let fixpoint (f,b) = wwhile ((fun b -> (f b, (f b) != b)), b)

(* ffor: int * int * (int -> unit) -> unit
   Applies the function f to all the integers between low and high
   inclusive; the results get thrown away.
 *)

let rec ffor (low,high,f) = 
  if low>high 
  then () 
  else let _ = f low in ffor (low+1,high,f)
      
(************** Add Testing Code Here ***************)
