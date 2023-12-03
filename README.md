# Toy Example of Reverse Mode AutoDiff

The `build` funciton within [lib.graph.ml](graph.ml) is the main function used to build computations.

This small example adds to values and computes their derivative

```ocaml
module Float_Graph = Graph.Make_Graph(Float_t)
let _ = Float_Graph.build (fun () ->
  let open Float_Graph in
  let a = value_of 2.0 in
  let b = value_of 1.0 in
  let c = a + a + b + b + a in
  backward c;
  let ga = grad a in
  let gb = grad b in
  print_endline @@ show ga;
  Printf.printf "Grad a: %f\n" (realize ga);
  (* Grad a: 3.000000 *)
  print_endline @@ show gb;
  Printf.printf "Grad b: %f\n" (realize gb);
  (* Grad b: 2.000000 *)
```

The backward pass is fully symbolic, and builds a new *backward graph* for gradient calculation at each node. These gradients must therefore be *realized*.
It is intended that an arbitrary backend will be able to do the backward and
forward graph calculations.