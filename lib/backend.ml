module type Backend = functor (DType : Types.Data_Type) -> sig
  type t

  val realize : t -> DType.t
end

(* Realize the operations performed through a simple interpreter *)
module Interp =
functor
  (DType : Types.Data_Type)
  ->
  struct
    module G = Graph.Make_Graph (DType)

    type t = DType.t

    (* let rec realize (graph : t G.expr) =
       let open DType in
       (* Allows us to use infix operations *)
       match graph with
       | Add (e1, e2) ->
       let v1 = realize e1 in
       let v2 = realize e2 in
       v1 + v2
       | Mul (e1, e2) ->
       let v1 = realize e1 in
       let v2 = realize e2 in
       v1 * v2
       | Atom (Const i) -> i
       | Atom (Val v) -> v *)
  end
