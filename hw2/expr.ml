(*
 * expr.ml
 * cse130
 * based on code by Chris Stone
 *)

(* Please do not modify the names or types of any of the following
 * type constructors, or predefined functions, unless EXPLICITLY
 * asked to. You will loose points if you do.
 *)


(* REMEMBER TO DOCUMENT ALL FUNCTIONS THAT YOU WRITE OR COMPLETE *)

type expr = 
    VarX
  | VarY
  | Sine     of expr
  | Cosine   of expr
  | Average  of expr * expr
  | Times    of expr * expr
  | Thresh   of expr * expr * expr * expr	
  | Sigmoid  of expr
  | Maximum  of expr * expr * expr

(* input: expr
 * output: string of expression
 * sin e -> sin(pi*e)
 * cos e -> cos(pi*e)
 * avg e1 e2 -> ((e1+e2)/2)
 * times e1 e2 -> e1*e2
 * thresh e1 e2 e3 e4 -> ((e1<e2)?e3:e4)
 * sigmoid e' -> (1/1+e^e'))
 *)
let rec exprToString e = 
  match e with 
  | VarX              -> "x"
  | VarY              -> "y"
  | Sine e'           -> "sin(pi*" ^ exprToString e' ^ ")"
  | Cosine e'         -> "cos(pi*" ^ exprToString e' ^ ")"
  | Average (e1, e2)  -> "((" ^ exprToString e1 ^ "+" ^ exprToString e2 ^ ")/2)"
  | Times (e1, e2)    -> exprToString e1 ^ "*" ^ exprToString e2
  | Thresh (e1, e2, e3, e4)   -> "(" ^ exprToString e1 ^ "<" ^ exprToString e2 ^ "?" ^ exprToString e3 ^ ":" ^ exprToString e4 ^ ")"
  | Sigmoid e'        -> "(1/(1+e^" ^ exprToString e' ^ "))"
  | Maximum (e1, e2, e3)      -> "max(" ^ exprToString e1 ^ "," ^ exprToString e2 ^ "," ^ exprToString e3 ^ ")"
;;


(* build functions:
     Use these helper functions to generate elements of the expr
     datatype rather than using the constructors directly.  This
     provides a little more modularity in the design of your program *)

let buildX()                       = VarX
let buildY()                       = VarY
let buildSine(e)                   = Sine(e)
let buildCosine(e)                 = Cosine(e)
let buildAverage(e1,e2)            = Average(e1,e2)
let buildTimes(e1,e2)              = Times(e1,e2)
let buildThresh(a,b,a_less,b_less) = Thresh(a,b,a_less,b_less)
let buildSigmoid(e)                = Sigmoid(e)
let buildMaximum(e1,e2,e3)         = Maximum(e1,e2,e3)

let pi = 4.0 *. atan 1.0

(* eval expression e with VarX as x and VarY as y
 * input: (expr e, float x, float y)
 * output: eval result of float
 *)
let rec eval (e,x,y) = 
  match e with 
  | VarX              -> x
  | VarY              -> y
  | Sine e'           -> sin(pi *. (eval (e', x, y)))
  | Cosine e'         -> cos(pi *. (eval (e', x, y)))
  | Average (e1, e2)  -> ((eval (e1, x, y)) +. (eval (e2, x, y))) /. 2.0
  | Times (e1, e2)    -> (eval (e1, x, y)) *. (eval (e2, x, y))
  | Thresh (e1, e2, e3, e4)   -> if (eval (e1, x, y)) < (eval (e2, x, y)) then (eval (e3, x, y)) else (eval (e4, x, y))
  | Sigmoid e'        -> 1.0 /. (1.0 +. exp (eval (e', x, y)))
  | Maximum (e1, e2, e3)      -> let (v1,v2,v3) = ((eval (e1, x, y)), (eval (e2, x, y)), (eval (e1, x, y))) in
                                  if v1 < v2 then (if v2 < v3 then v3 else v2)
                                  else (if v1 < v3 then v3 else v1) 
;;

(* (eval_fn e (x,y)) evaluates the expression e at the point (x,y) and then
 * verifies that the result is between -1 and 1.  If it is, the result is returned.  
 * Otherwise, an exception is raised.
 *)
let eval_fn e (x,y) = 
  let rv = eval (e,x,y) in
  assert (-1.0 <= rv && rv <= 1.0);
  rv

let sampleExpr =
      buildCosine(buildSine(buildTimes(buildCosine(buildAverage(buildCosine(
      buildX()),buildTimes(buildCosine (buildCosine (buildAverage
      (buildTimes (buildY(),buildY()),buildCosine (buildX())))),
      buildCosine (buildTimes (buildSine (buildCosine
      (buildY())),buildAverage (buildSine (buildX()), buildTimes
      (buildX(),buildX()))))))),buildY())))

let sampleExpr2 =
  buildThresh(buildX(),buildY(),buildSine(buildX()),buildCosine(buildY()))


(************** Add Testing Code Here ***************)
