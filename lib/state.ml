module type STATE = sig
  type t

  val withState : init:t -> ('a -> 'b) -> 'a -> 'b * t
  val get : unit -> t
  val put : t -> unit
  val modify : (t -> t) -> unit
end

module Make (M : sig
    type t
  end) : STATE with type t = M.t = struct
  open Effect
  open Effect.Deep

  type t = M.t
  type 'a state = { s : t -> 'a * t }
  type _ Effect.t += Get : t Effect.t | Put : t -> unit Effect.t
  (* | State : (t -> 'a * t) -> unit Effect.t *)

  let put v = perform (Put v)
  let get () = perform Get

  let modify (f : t -> t) =
    let s = get () in
    put (f s)
  ;;

  let withState ~init (comp : 'a -> 'b) (a : 'a) =
    let state_builder =
      match_with
        comp
        a
        { exnc = raise
        ; retc = (fun r -> { s = (fun e -> r, e) })
        ; effc =
            (fun (type b) (eff : b Effect.t) ->
              match eff with
              | Get ->
                Some
                  (fun (k : (b, _) continuation) ->
                    { s =
                        (fun x ->
                          let cur_state = continue k x in
                          cur_state.s x)
                    })
              | Put v ->
                Some
                  (fun (k : (b, _) continuation) ->
                    { s =
                        (fun _ ->
                          let cur_state = continue k () in
                          cur_state.s v)
                    })
              | _ -> None)
        }
    in
    state_builder.s init
  ;;
end

(* module Assoc_List = struct
   type t = (int * int) list
   end

   module Assoc_State = State(Assoc_List) *)
