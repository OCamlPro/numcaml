open Tests
open Numcaml

let test_add () =
  let x = $(1 + 2) in
  let y = $(3)  in
  "values match" @? (x=y)

let test_mul () =
  let x = $(1+2*3) in
  let y = $(7) in
  "value match" @? (x=y)

let test_aq () =
  let x = 3 in
  let y = $(4 + (x:int)) in
  let z = 7 in
  "value match" @? (y = $( (z:int) ))

let suite = [
  "int_add" >:: test_add;
  "int_mul" >:: test_mul;
  "int_aq"  >:: test_aq;
]
