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

let sf = Printf.sprintf

type t =
  | Int
  | Int32
  | Int64
  | Float
  | Complex
  | Vector of vector
  | Matrix of matrix

(* XXX: we can optimize representation of 'a vector ... vector *)
and vector = {
  v_t : t;
  v_n : int; (* XXX: in this case, this should be v_shape: int list *)
}

and matrix = {
  m_t : t;
  m_n : int;
  m_m : int;
}

let rec to_string = function
  | Int      -> "int"
  | Int32    -> "int32"
  | Int64    -> "int64"
  | Float    -> "float"
  | Complex  -> "complex"
  | Vector v -> sf "%s vector[%d]" (to_string v.v_t) v.v_n
  | Matrix m -> sf "%s matrix[%d,%d]" (to_string m.m_t) m.m_m m.m_n

(* Find the smallest type consistent with t1 and t2 *)
let rec top t1 t2 =
  if t1=t2 then t1 else
    match (t1, t2) with
    (* Vector/Matrix *)
    | Vector x, Vector y -> Vector { v_t = top x.v_t y.v_t; v_n = 0 }
    | Vector x, Matrix y
    | Matrix y, Vector x -> Matrix { m_t = top x.v_t y.m_t; m_n = 0; m_m = 0 }
    | Matrix x, Matrix y -> Matrix { m_t = top x.m_t y.m_t; m_n = 0; m_m = 0 }
    | Vector x, t
    | t       , Vector x -> Vector { x with v_t = top t x.v_t }
    | Matrix x, t
    | t       , Matrix x -> Matrix { x with m_t = top t x.m_t }

    (* Complex *)
    | Complex, Int
    | Complex, Int32
    | Complex, Int64
    | Complex, Float
    | Int    , Complex
    | Int32  , Complex
    | Int64  , Complex
    | Float  , Complex -> Complex

    (* Float *)
    | Float, Int
    | Float, Int32
    | Float, Int64
    | Int  , Float
    | Int32, Float
    | Int64, Float -> Float

    (* Int64 *)
    | Int64, Int
    | Int64, Int32
    | Int  , Int64
    | Int32, Int64 -> Int64

    (* Int32 *)
    | Int32, Int
    | Int  , Int32 -> Int32

    | _ -> assert false
  
