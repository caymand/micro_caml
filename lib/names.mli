(* Effectful computation that creates fresh identifiers. *)

val fresh : ?base_name:string -> unit -> string
val with_fresh : (string -> unit) -> unit
