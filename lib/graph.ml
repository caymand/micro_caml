module type Graph = sig
  type dtype
  type t [@@deriving show]

  type atom =
    | Zero
    | One
    | Ident of string (* Identifier variable *)
    | Adjoint of string (* Adjoint variable. TODO: remove *)
    | Val of dtype
    | Const of dtype
  [@@deriving show]

  type expr =
    | Add of t * t
    | Mul of t * t
    | Sub of t * t
    | Cos of t
    | Sin of t
    | Atom of atom
  [@@deriving show]

  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( - ) : t -> t -> t
  val cos : t -> t
  val sin : t -> t
  val value_of : dtype -> t
  val const : dtype -> t
  val build : (unit -> unit) -> unit
  val backward : t -> unit
  val grad : t -> t
  val realize : t -> dtype
  val print_graph : unit -> unit
end

module Make_Graph (DType : Types.Data_Type) : Graph with type dtype = DType.t = struct
  (* Underlying type of the nodes in the graph *)
  type dtype = DType.t [@@deriving show]

  (* DSL to wrok on the graph *)
  type t =
    { tag : string
    ; expr : expr
    }
  [@@deriving.show]

  and atom =
    | Zero
    | One
    | Ident of string
    | Adjoint of string
    | Val of dtype
    | Const of dtype
  [@@deriving show]

  and expr =
    | Add of t * t
    | Mul of t * t
    | Sub of t * t
    | Cos of t
    | Sin of t
    | Atom of atom
  [@@deriving show]



  (* type tape = node list *)

  let one = Atom One

  let zero = Atom Zero
  let make_val tag expr = { tag; expr }

  let make_op expr base_name =
    let op_name = Names.fresh ~base_name () in
    make_val op_name expr
  ;;

  let ( + ) v1 v2 =
    let e = Add (v1, v2) in
    make_op e "add"
  ;;

  let ( * ) v1 v2 =
    let e = Mul (v1, v2) in
    make_op e "mul"
  ;;

  let ( - ) e1 e2 =
    let e = Sub (e1, e2) in
    make_op e "sub"
  ;;

  let cos e = make_op (Cos e) "cos"
  let sin e = make_op (Sin e) "sin"

  let value_of v =
    let name = Names.fresh ()
    and expr = Atom (Val (DType.value_of v)) in
    make_val name expr
  ;;

  let const v =
    let name = Names.fresh ~base_name:"const_" ()
    and expr = Atom (Const (DType.value_of v)) in
    make_val name expr
  ;;

  let fresh_tag base_name = Names.fresh ~base_name ()

  let fresh_ident tag =
    let tag' = Names.fresh ~base_name:tag () in
    make_val tag' (Atom (Ident tag'))
  ;;

  let bind_in lhs rhs env = (lhs, rhs) :: env

  (* Do BFS traversal of the graph.*)
  let walk_comp graph =
    let to_visit = Queue.create () in
    let rec bfs visited node =
      (match node.expr with
       | Add (v1, v2) ->
         Queue.add v1 to_visit;
         Queue.add v2 to_visit
       | Mul (v1, v2) ->
         Queue.add v1 to_visit;
         Queue.add v2 to_visit
       | Sub (v1, v2) ->
         Queue.add v1 to_visit;
         Queue.add v2 to_visit
       | Cos v -> Queue.add v to_visit
       | Sin v -> Queue.add v to_visit
       | Atom _ -> ());
      let visited' = node :: visited in
      if Queue.is_empty to_visit then visited' else bfs visited' (Queue.pop to_visit)
    in
    bfs [] graph
  ;;

  (* A tape is the tuple string * t.
     That is, the name of the node and an expression.
     It can be seen as the assignment node = expr.*)
  let make_tape topo_sort =
    let bin_op op e1 e2 ident nodes =
      let e1' = List.assoc e1.tag nodes in
      let e2' = List.assoc e2.tag nodes in
      let expr = op e1' e2' in
      let ident' = fresh_ident "z_" in
      (* Old ident becomes a reference to the new identifier.
         The new identifier now becomes the expression:
         x = x'; x' = op e1 e2.
         This gives us a flat tape where each occurence of x' will
         just be an Ident.
      *)
      (ident, ident') :: (ident'.tag, expr) :: nodes
    in
    let unary_op op e ident nodes =
      let ident' = fresh_ident "z_" in
      let e' = List.assoc e.tag nodes in
      let expr = make_val (fresh_tag "z_") @@ op e' in
      let nodes' = bind_in ident'.tag expr nodes in
      bind_in ident ident' nodes'
    in
    let tape =
      List.fold_left
        (fun nodes node ->
          match node.expr with
          | Atom (Val _) -> (node.tag, node) :: nodes
          | Atom (Const _) -> (node.tag, node) :: nodes
          | Atom _ -> failwith "Needs refactor"
          | Cos e -> unary_op (fun e' -> Cos e') e node.tag nodes
          | Sin e -> unary_op (fun e' -> Sin e') e node.tag nodes
          | Add (e1, e2) -> bin_op ( + ) e1 e2 node.tag nodes
          | Mul (e1, e2) -> bin_op ( * ) e1 e2 node.tag nodes
          | Sub (e1, e2) -> bin_op ( - ) e1 e2 node.tag nodes)
        []
        topo_sort
    in
    (* Remove all the nodes that were renamed. *)
    List.filter
      (fun (_, e) ->
        match e.expr with
        | Atom (Ident _) -> false
        | _ -> true)
      tape
  ;;

  (* Woudl be prettifer with effect that resembles reader monad*)
  module Grad_Env = struct
    type u = t
    type tape = (string * u) list

    type t =
      { forward_tape : tape
      ; backward_tape : tape (* Stores the adjoint variables *)
      }
  end

  module Grads = State.Make (Grad_Env)
  (* TODO: Initialize all variables and simply accumulate.
     That way we do not need the branch.*)
  let update_grad z v op =
    Grads.modify (fun state ->
      let grads = state.backward_tape in
      let grads' =
        (* Instead of copying the grad over, make a reference to the adjoint.*)
        let gz = make_val (fresh_tag (z ^ "z_")) (Atom (Adjoint z)) in
        match List.assoc_opt v grads with
        | None -> bind_in v (gz * op v) grads
        | Some gv ->
          let grads' = List.remove_assoc v grads in
          let gv' = gv + (gz * op v) in
          bind_in v gv' grads'
      in
      { state with backward_tape = grads' })
  ;;

  let backward' tape =
    List.iter
      (fun (parent, node) ->
        match node.expr with
        | Atom (Const _) -> ()
        | Atom (Val _) -> ()
        | Add (e1, e2) ->
          update_grad parent e1.tag (fun _ -> make_val "one_" one);
          update_grad parent e2.tag (fun _ -> make_val "one_" one)
        | Mul (e1, e2) ->
          update_grad parent e1.tag (fun _ -> e2);
          update_grad parent e2.tag (fun _ -> e1)
        | Sub (e1, e2) ->
          (* TODO: this should be mul in the future. *)
          update_grad parent e1.tag (fun _ -> make_val "one_" one);
          update_grad parent e2.tag (fun _ -> make_val "one_" one)
        | Sin e ->
          update_grad parent e.tag (fun _ -> { node with expr = Cos e })
        | Cos e ->
          update_grad parent e.tag (fun _ -> { node with expr = Sin e })
            
        | _ -> failwith "Incorrect tape")
      tape
  ;;

  let backward graph =
    let tape = walk_comp graph |> make_tape in
    (* Seed the gradient of the last node.*)
    let last_node, _ = List.hd tape in
    let seed = make_val "seed" one in
    (* Set the last node equal to the seed *)
    Grads.put { forward_tape = tape; backward_tape = bind_in last_node seed [] };
    backward' tape
  ;;

  let grad leaf =
    match List.assoc_opt leaf.tag (Grads.get ()).backward_tape with
    | Some g -> g
    | None -> failwith "Node not in graph"
  ;;

  let build (comp : unit -> 'a) =
    Names.with_fresh (fun _ ->
      let state : Grads.t = { forward_tape = []; backward_tape = [] } in
      let res, _final_state = Grads.withState ~init:state comp () in
      res)
  ;;
  let print_graph () = let graph = Grads.get () in
    print_endline @@ [%show: (string * t) list ] graph.backward_tape


  let realize grad =
    let state = Grads.get () in
    let open DType in
    let rec realize' grad =
      match grad.expr with
      | Atom Zero -> zero
      | Atom One -> one
      | Atom (Const v) -> v
      | Atom (Val v) -> v
      | Atom (Ident v) ->
        let v_expr = List.assoc v state.forward_tape in
        realize' v_expr
      | Atom (Adjoint v) ->
        let v_expr = List.assoc v state.backward_tape in
        realize' v_expr
      | Mul (v1, v2) -> realize' v1 * realize' v2
      | Add (v1, v2) -> realize' v1 + realize' v2
      | Sub (v1, v2) -> realize' v1 - realize' v2
      | Cos v -> cos (realize' v)
      | Sin v -> sin (realize' v)
    in
    realize' grad
  ;;
end
