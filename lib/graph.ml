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
    | Atom of atom
  [@@deriving show]

  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
  val value_of : dtype -> t
  val const : dtype -> t
  val walk_comp : t -> t list
  val make_tape : t list -> (string * t) list
  val build : (unit -> unit) -> unit
  val backward : t -> unit
  val grad : t -> t
  val realize : t -> dtype
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
    | Atom of atom
  [@@deriving show]

  (* type tape = node list *)

  let one = Atom One
  (* let zero = Atom Zero *)
  let make_val tag expr = { tag; expr }

  let ( + ) v1 v2 =
    let e = Add (v1, v2)
    and op_name = Names.fresh ~base_name:"add" () in
    make_val op_name e
  ;;

  let ( * ) v1 v2 =
    let e = Mul (v1, v2)
    and op_name = Names.fresh ~base_name:"mul" () in
    make_val op_name e
  ;;

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
       | _ -> ());
      let visited' = node :: visited in
      if Queue.is_empty to_visit then visited' else bfs visited' (Queue.pop to_visit)
    in
    bfs [] graph
  ;;

  let fresh_tag base_name = Names.fresh ~base_name ()

  let fresh_ident tag =
    let tag' = Names.fresh ~base_name:tag () in
    make_val tag' (Atom (Ident tag'))
  ;;

  (* A tape is the tuple string * t.
     That is, the name of the node and an expression.
     It can be seen as the assignment node = expr.*)
  let make_tape topo_sort =
    let tape =
      List.fold_left
        (fun nodes node ->
          match node.expr with
          | Atom (Val _) -> (node.tag, node) :: nodes
          | Atom (Const _) -> (node.tag, node) :: nodes
          | Add (e1, e2) ->
            let e1' = List.assoc e1.tag nodes in
            let e2' = List.assoc e2.tag nodes in
            let rhs = e1' + e2' in
            let lhs = fresh_ident "z_" in
            (node.tag, lhs) :: (lhs.tag, rhs) :: nodes
          | Mul (e1, e2) ->
            let e1' = List.assoc e1.tag nodes in
            let e2' = List.assoc e2.tag nodes in
            let rhs = e1' * e2' in
            let lhs = fresh_ident "z_" in
            (node.tag, lhs) :: (lhs.tag, rhs) :: nodes
          | _ -> failwith "")
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
      { forward_tape : tape (* Stores the adjoint variables *)
      ; backward_tape : tape
      }
  end

  module Grads = State.Make (Grad_Env)

  let update_grad z v op =
    Grads.modify (fun state ->
      let grads = state.backward_tape in
      let grads' =
        (* Instead of copying the grad over, make a reference to the adjoint.*)
        let gz = make_val (fresh_tag @@ z ^ "_") @@ Atom (Adjoint z) in
        match List.assoc_opt v grads with
        | None -> (v, gz * op v) :: grads
        | Some gv ->
          let grads' = List.remove_assoc v grads in
          let gv' = gv + (gz * op v) in
          (v, gv') :: grads'
      in
      { state with backward_tape = grads' })
  ;;

  (* The problem is mixing backwards and forwards nodes. *)
  let backward' tape =
    List.iter
      (fun (z, node) ->
        match node.expr with
        | Atom (Const _) -> ()
        | Atom (Val _) -> ()
        | Add (e1, e2) ->
          update_grad z e1.tag (fun _ -> make_val "one_" one);
          update_grad z e2.tag (fun _ -> make_val "one_" one)
        | Mul (e1, e2) ->
          update_grad z e1.tag (fun _ -> e2);
          update_grad z e2.tag (fun _ -> e1)
        | _ -> failwith "Incorrect tape")
      tape
  ;;

  (* This simply builds the backwards tape. It then has to be realized *)
  let backward graph =
    let tape = walk_comp graph |> make_tape in
    (* Seed the gradient of the last node.*)
    let last_node, _ = List.hd tape in
    let seed = make_val "seed" one in
    (* Set the last node equal to the seed *)
    Grads.put { forward_tape = tape; backward_tape = [ last_node, seed ] };
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
    in
    realize' grad
  ;;
end
