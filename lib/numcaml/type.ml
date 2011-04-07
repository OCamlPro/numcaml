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

type t =
  | Int
  | Int32
  | Int64
  | Float
  | Complex
  | Vector of t
  | Matrix of t

let rec to_string = function
  | Int      -> "int"
  | Int32    -> "int32"
  | Int64    -> "int64"
  | Float    -> "float"
  | Complex  -> "complex"
  | Vector t -> to_string t ^ " vector"
  | Matrix t -> to_string t ^ " matrix"

(* Are value of type t1 can be upcasted to value of type t2 ? *)
let rec upcastable t1 t2 =
  match (t1, t2) with
    (* Vector/Matrix *)
    | Vector x, Vector y
    | Vector x, Matrix y
    | Matrix x, Matrix y -> upcastable x y
    | Vector _, _ 
    | Matrix _, _        -> false

    (* Int *)
    | Int, _ -> true

    (* Int32 *)
    | Int32, Int -> false
    | Int32, _   -> true

    (* Int64 *)
    | Int64, Int
    | Int64, Int32 -> false
    | Int64, _     -> true

    (* Float *)
    | Float, Int
    | Float, Int32
    | Float, Int64 -> false
    | Float, _     -> true

    (* Complex *)
    | Complex, Int
    | Complex, Int32
    | Complex, Int64
    | Complex, Float -> false
    | Complex, _     -> true
