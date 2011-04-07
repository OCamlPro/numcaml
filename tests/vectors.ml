open OUnit
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

let suite = [
  "vector_add" >:: test_add;
  "vector_mul" >:: test_mul;
]
