open Micro_caml.Graph
open Micro_caml.Types
module Float_Graph = Make_Graph (Float_t)

let%expect_test "Test add with constant" =
  let open Float_Graph in
  let add_const () = build (fun () ->
      let a = value_of 1.0 in
      let comp = (const 2.0) + a in
      backward comp;
      print_float @@ realize (grad a)
    );
    [%expect{| 1. |}]
  in
  let add_const_var () = build (fun () ->
      let a = value_of 1.0 in
      let b = value_of 2.0 in
      let comp = a + b + (const 2.0) in
      backward comp;
      print_float @@ realize (grad a);
      print_float @@ realize (grad b)
    );
    [%expect{| 1.1. |}]
  in
  add_const ();
  add_const_var ()

let%expect_test "Test mul constant" =
  let open Float_Graph in
  build (fun () ->
      let a = value_of 2.0 in
      let comp1 = a * (const 2.0) in
      let comp2 = a * (const 1.0) in
      backward comp1;
      print_float @@ realize (grad a);
      (* This will reset the gradients. Do we want that?*)
      backward comp2;
      print_float @@ realize (grad a)
    );
  [%expect{| 2.1. |}]

let compound_expr () =
  let open Float_Graph in
  build (fun () ->
    let a = value_of 4.0 in
    let comp = a * a * a in
    backward comp;
    let ga = grad a in
    (* print_endline @@ show ga; *)
    print_float @@ realize ga)
;;

let%expect_test "Test compound expression" =
  compound_expr ();
  [%expect {|48.|}]
;;
