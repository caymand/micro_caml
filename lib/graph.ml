module type Graph = sig
  type dtype
  type t [@@deriving show]

  type atom =
    | Zero
    | One
    | Ident of string
    | Val of dtype
    | Const of dtype
  [@@deriving show]

  type expr =
    | Add of t * t
    | Mul of t * t
    | Atom of atom
  [@@deriving show]

  type node =
    { value : t
    ; parents : string list
    ; grad : expr
    }
  [@@deriving show]

  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
  val value_of : dtype -> t
  val const : dtype -> t
  val traverse : t -> t list
  val make_tape : t list -> (string * t) list
  val backward : (string * t) list -> (string * t) list
  val realize : t -> (string * t) list -> dtype
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
    | Val of dtype
    | Const of dtype
  [@@deriving show]

  and expr =
    | Add of t * t
    | Mul of t * t
    | Atom of atom
  [@@deriving show]

  type node =
    { value : t
    ; parents : string list
    ; grad : expr
    }
  [@@deriving show]

  (* type tape = node list *)

  let one = Atom One
  let zero = Atom Zero
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

  let traverse graph =
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

  let fresh_val tag =
    let tag' = Names.fresh ~base_name:tag () in
    make_val tag' (Atom (Ident tag'))
  ;;

  (* TODO: Handle exceptions. Maybe use effects and make the state take an exception handler*)

  (* A tape is the tuple string * t. That is, the name of the node and an expression.
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
            let lhs = fresh_val "z_" in
            (* (lhs.tag, rhs) :: nodes *)
            (node.tag, lhs) :: (lhs.tag, rhs) :: nodes
          | Mul (e1, e2) ->
            let e1' = List.assoc e1.tag nodes in
            let e2' = List.assoc e2.tag nodes in
            let rhs = e1' * e2' in
            let lhs = fresh_val "z_" in
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

  module Grad_Env = struct
    type u = t [@@deriving show]
    type t = (string * u) list [@@deriving show]
  end

  module Grads = State.Make (Grad_Env)

  (*TODO: might be useful to have functional environments instead of association lists*)
  let update_grad z v op =
    Grads.modify (fun grads ->
      (* Instead of copying the grad over, make a reference to it.*)
      let gz = make_val (fresh_tag @@ z ^ "_") @@ Atom (Ident z) in
      match List.assoc_opt v grads with
      | None -> (v, gz * op v) :: grads
      | Some gv ->
        let grads' = List.remove_assoc v grads in
        let gv' = gv + (gz * op v) in
        (v, gv') :: grads')
  ;;

  let backward' tape =
    List.iter
      (fun (z, node) ->
        match node.expr with
        | Atom (Const _) -> ()
        | Atom (Val _) -> ()
        | Add (e1, e2) ->
          update_grad z e1.tag (fun _ -> make_val "one_" one);
          update_grad z e2.tag (fun _ -> make_val "one_" one)
        | Atom (Ident _) ->
          (* All identifiers shoudl only appear within compound statements.
             As such this should never happesn.*)
          failwith "Incorrect tape"
        | _ -> failwith "")
      tape
  ;;

  let backward tape =
    let last_node, _ = List.hd tape in
    let _, grads =
      Grads.withState ~init:[ last_node, make_val "grad_" one ] backward' tape
    in
    grads
  ;;

  let rec realize grad all_grads =
    match grad.expr with
    | Atom Zero -> DType.zero
    | Atom One -> DType.one
    | Atom (Const v) -> v
    | Atom (Val v) -> v
    | Atom (Ident z) ->
      let gz = List.assoc z all_grads in
      realize gz all_grads
    | Mul (v1, v2) -> DType.( * ) (realize v1 all_grads) (realize v2 all_grads)
    | Add (v1, v2) -> DType.( + ) (realize v1 all_grads) (realize v2 all_grads)
  ;;
end
