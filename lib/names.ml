open Effect
open Effect.Shallow

type _ Effect.t += Fresh_Name : string -> string Effect.t

let fresh ?(base_name = "x_") () = perform (Fresh_Name base_name)

let with_fresh comp =
  let base_name = "x_" in
  let rec create_generator count name (k : (string, unit) continuation) =
    continue_with
      k
      name
      { retc = Fun.id
      ; exnc = raise
      ; effc =
          (fun (type b) (eff : b Effect.t) ->
            match eff with
            | Fresh_Name base_name ->
              Some
                (fun (k : (b, _) continuation) ->
                  let new_name = base_name ^ Int.to_string count in
                  create_generator (count + 1) new_name k)
            | _ -> None)
      }
  in
  create_generator 0 base_name (fiber comp)
;;
