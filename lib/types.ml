(* Primitive types that the graph is built from.*)

module type Data_Type = sig
  type t

  val pp : Format.formatter -> t -> unit
  val zero : t
  val one : t
  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( - ) : t -> t -> t
  val sin : t -> t
  val cos : t -> t
  val value_of : t -> t
end

module Float_t : Data_Type with type t = float = struct
  type t = float [@@deriving show]

  let zero = 0.0
  let one = 1.0
  let ( + ) = Float.add
  let ( * ) = Float.mul
  let (-) = Float.sub
  let sin = Float.sin
  let cos = Float.cos
  let value_of = Fun.id
end
