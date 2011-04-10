open Tests
open Numcaml

let one = Math.ones [10]

let test_add () =
  let x = $(3.3 + one)  in
  let y = Math.Vector (Array.create 10 (Math.Float 4.3)) in
  "values match" @? (x=y)

let test_mul () =
  let x = $(7l * one) in
  let y = Math.Vector (Array.create 10 (Math.Int32 7l)) in
  "value match" @? (x=y)

let test_q () =
  let x = $( [| 1; 2l; 3; 4 |] ) in
  let y = $( x + 0l ) in
  let z = Math.Vector (Array.init 4 (fun i -> Math.Int32 (Int32.of_int (i+1)))) in
  "value match" @? (y = z)

let test_aq () =
  let x = Array.init 10 (fun i -> i) in
  let y = $( 10.5 + (x : int vector)) in
  let z = Array.init 10 (fun i -> float i +. 10.5) in
  "value match" @? (y = $( (z : float vector) ) )

let suite = [
  "vector_add" >:: test_add;
  "vector_mul" >:: test_mul;
  "vector_aq"  >:: test_aq;
  "vector_q"   >:: test_q;
]
