(*
 * Copyright (c) 2011 Thomas Gazagnaire <thomas@ocamlpro.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

type 'a t = 'a array array

let make = Array.make_matrix

let check_size x y =
  Vector.check_size x y;
  Vector.check_size x.(0) y.(0)

let map fn x =
  Array.map (fun x0 ->
    Array.map (fun e -> fn e) x0
  ) x

let map2 fn x y =
  Array.mapi (fun i x0 ->
    Array.mapi (fun j e -> fn e y.(i).(j)) x0
  ) y

let shape x =
  Array.length x, Array.length x.(0)

let mul _add _mul x y =
  let a, m = shape x in
  let n, b = shape y in

  if m <> n then
    Error.not_aligned ();

  let c = Array.create_matrix a b x.(0).(0) in

  (* XXX: use C/Fortran bindings here *)
  for i = 0 to a-1 do
    for j = 0 to b-1 do
      c.(i).(j) <- _mul x.(i).(0) y.(0).(j);
      for k = 1 to n-1 do
	c.(i).(j) <- _add c.(i).(j) (_mul x.(i).(k) y.(k).(j))
      done
    done
  done;

  c
