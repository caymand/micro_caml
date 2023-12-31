open Micro_caml.Graph
open Micro_caml.Types
module Float_Graph = Make_Graph (Float_t)

let%expect_test "Test add with constant" =
  let open Float_Graph in
  let add_const () =
    build (fun () ->
      let x = value_of 1.0 in
      let f = const 2.0 + x in
      backward f;
      print_float @@ realize (grad x));
    [%expect {| 1. |}]
  in
  let add_const_var () =
    build (fun () ->
      let a = value_of 1.0 in
      let b = value_of 2.0 in
      let comp = a + b + const 2.0 in
      backward comp;
      print_float @@ realize (grad a);
      print_float @@ realize (grad b));
    [%expect {| 1.1. |}]
  in
  add_const ();
  add_const_var ()
;;

let%expect_test "Test mul constant" =
  let open Float_Graph in
  build (fun () ->
    let a = value_of 2.0 in
    let comp1 = a * const 2.0 in
    let comp2 = a * const 1.0 in
    backward comp1;
    print_float @@ realize (grad a);
    (* This will reset the gradients. Do we want that?*)
    backward comp2;
    print_float @@ realize (grad a));
  [%expect {| 2.1. |}]
;;

let compound_expr () =
  let open Float_Graph in
  build (fun () ->
    let a = value_of 4.0 in
    let b = value_of 3.0 in
    let c = value_of 2.0 in
    let comp = (a * (a + c) * a) + (const 2.0 * a * b) in
    backward comp;
    let ga = grad a in
    let gb = grad b in
    let gc = grad c in
    print_float @@ realize ga;
    print_float @@ realize gb;
    print_float @@ realize gc)
;;

let%expect_test "Test compound expression" =
  compound_expr ();
  [%expect {|70.8.16.|}]
;;

let%expect_test "Unary ops" =
  let open Float_Graph in
  let test_unary_op op = build (fun () ->
    let a = value_of (Float.div Float.pi 2.0) in
    let comp = (const 2.0) * op a in
    backward comp;
    let ga = grad a in
    Printf.printf "%f " (realize ga)) in
  test_unary_op sin;
  test_unary_op cos;
  [%expect {|0.000000 2.000000 |}]
;;
