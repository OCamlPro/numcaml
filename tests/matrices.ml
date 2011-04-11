open Tests
open Numcaml

let id = Math.id 2
let ones = Math.ones [2;2]

let test_mul () =
  let x = $( 2.4 * id ) in
  let y = $( Math.matrix [| [| 2;3 |]; [| 4;5 |] |] ) in
  let _ : Math.t = $( x*y + ones*4l ) in
  ()

let suite = [
  "matrix_mul" >:: test_mul;
]
