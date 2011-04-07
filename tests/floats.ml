open OUnit
open Numcaml

let test_add () =
  let x = $(1.4 + 2) in
  let y = $(3.4)  in
  "values match" @? (x=y)

let test_mul () =
  let x = $(1.3+2*3.1) in
  let y = Math.Float (1.3 +. 2. *. 3.1) in
  "value match" @? (x=y)

let suite = [
  "int_add" >:: test_add;
  "int_mul" >:: test_mul;
]
