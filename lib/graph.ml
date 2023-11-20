module type Graph = sig
  type dtype
  type t [@@deriving show]

  type atom =
    | Zero
    | One
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
  val traverse : t -> node list
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

  module Value_Env = struct
    type u = t [@@deriving show]
    type t = (string * u) list [@@deriving show]
  end

  module Tags = State.Make (Value_Env)

  let rename expr_tag =
    match List.assoc_opt expr_tag (Tags.get ()) with
    | None ->
      let expr'_tag = Names.fresh ~base_name:"z_" () in
      let expr' = make_val expr'_tag zero in
      Tags.modify (fun cur_tags -> (expr_tag, expr') :: cur_tags);
      expr'
    | Some value -> value
  ;;

  let make_node value parents grad =
    let expr =
      match value.expr with
      | Add (v1, v2) ->
        let v1' = rename v1.tag
        and v2' = rename v2.tag in
        Add (v1', v2')
      | Mul (v1, v2) ->
        let v1' = rename v1.tag
        and v2' = rename v2.tag in
        Mul (v1', v2')
      | Atom _ -> value.expr
    in
    (* Check if we already have renamed this variable. In that case
       we need to refer to this renamed value. *)
    let tag =
      match List.assoc_opt value.tag (Tags.get ()) with
      | Some value' -> value'.tag
      | None ->
        (* value.tag *)
        (rename value.tag).tag
    in
    let value = make_val tag expr in
    { value; parents; grad }
  ;;

  (* Traverse the graph from the root and produce a Wengert List*)
  let traverse root =
    let to_visit = Queue.create () in
    Queue.push ([], root) to_visit;
    let rec bfs value parent visited =
      (* Augment the value in the graph by a node.
         The node will be renamed so all computations needs to use this new name.*)
      let value' = make_node value parent zero in
      let _ =
        match value.expr with
        | Add (v1, v2) ->
          Queue.push ([ value'.value.tag ], v1) to_visit;
          Queue.push ([ value'.value.tag ], v2) to_visit
        | Mul (v1, v2) ->
          Queue.push ([ value'.value.tag ], v1) to_visit;
          Queue.push ([ value'.value.tag ], v2) to_visit
        | _ -> ()
      in
      let visited' = value' :: visited in
      if Queue.is_empty to_visit
      then visited'
      else (
        let parent, next_node = Queue.pop to_visit in
        bfs next_node parent visited')
    in
    let traversal, env =
      Tags.withState
        ~init:[]
        (fun () ->
          let _, n = Queue.pop to_visit in
          bfs n [] [] |> List.rev |> List.tl |> List.cons (make_node root [ "" ] one))
        ()
    in
    print_endline "Last state";
    print_endline @@ [%show: Value_Env.t] env;
    traversal
  ;;

  (* Derivative wrt. dependent variable*)
  let backward_op node dependent_var =
    match node.value.expr with
    | Add _ -> one
    | Mul _ -> one
    | Atom (Const _) -> zero
    | Atom (Val v) -> if dependent_var == node.value.tag then Atom (Const v) else zero
    | _ -> failwith "Not implemented"
  ;;

  let backward graph =
    (* The traversal that produces the tape walks the graph in a BFS manner.
       Therefore, when we reach a node, all previously visited nodes are either successors
       or some ancestor.*)
    let tape = traverse graph in
    tape
  ;;
end
