

type dtype

(* Leaf nodes can be either some constant, or a variable to which the
   derivative is calculated.*)
type atom =
  | Zero of dtype
  | One of dtype
  | Const of dtype
  | Var of string

type expr =
  | Atom : atom -> 

