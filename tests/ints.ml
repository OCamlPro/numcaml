open OUnit
open Numcaml

let test_add () =
  let x = $(1 + 2) in
  let y = $(3)  in
  "values match" @? (x=y)

let test_mul () =
  let x = $(1+2*3) in
  let y = $(7) in
  "value match" @? (x=y)

let suite = [
  "int_add" >:: test_add;
  "int_mul" >:: test_mul;
]
