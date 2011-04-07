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
  | Int     of int
  | Int32   of int32
  | Int64   of int64
  | Float   of float
  | Complex of Complex.t
  | Vector  of t Vector.t
  | Matrix  of t Matrix.t

let rec type_of_t = function
  | Int     _ -> Type.Int
  | Int32   _ -> Type.Int32
  | Int64   _ -> Type.Int64
  | Float   _ -> Type.Float
  | Complex _ -> Type.Complex
  | Vector  x -> Type.Vector (type_of_t x.(0))
  | Matrix  x -> Type.Matrix (type_of_t x.(0).(0))

let rec upcast v t = match (v, t) with

  (* Int *)
  | Int _, Type.Int      -> v
  | Int x, Type.Int32    -> Int32 (Int32.of_int x)
  | Int x, Type.Int64    -> Int64 (Int64.of_int x)
  | Int x, Type.Float    -> Float (float x)
  | Int x, Type.Complex  -> Complex { Complex.re = float x; im = 0. }
  | Int _, Type.Vector u -> Vector (Vector.create 1 (upcast v u))
  | Int _, Type.Matrix u -> Matrix (Matrix.create 1 1 (upcast v u))

  (* Int32 *)
  | Int32 _, Type.Int32    -> v
  | Int32 x, Type.Int64    -> Int64 (Int64.of_int32 x)
  | Int32 x, Type.Float    -> Float (Int32.to_float x)
  | Int32 x, Type.Complex  -> Complex { Complex.re = Int32.to_float x; im = 0. }
  | Int32 _, Type.Vector u -> Vector (Vector.create 1 (upcast v u))
  | Int32 _, Type.Matrix u -> Matrix (Matrix.create 1 1 (upcast v u))

  (* Int64 *)
  | Int64 _, Type.Int64    -> v
  | Int64 x, Type.Float    -> Float (Int64.to_float x)
  | Int64 x, Type.Complex  -> Complex { Complex.re = Int64.to_float x; im = 0. }
  | Int64 _, Type.Vector u -> Vector (Vector.create 1 (upcast v u))
  | Int64 _, Type.Matrix u -> Matrix (Matrix.create 1 1 (upcast v u))

  (* Float *)
  | Float _, Type.Float    -> v
  | Float x, Type.Complex  -> Complex { Complex.re = x; im = 0. }
  | Float _, Type.Vector u -> Vector (Vector.create 1 (upcast v u))
  | Float _, Type.Matrix u -> Matrix (Matrix.create 1 1 (upcast v u))

  (* Complex *)
  | Complex x, Type.Complex  -> v
  | Complex _, Type.Vector u -> Vector (Vector.create 1 (upcast v u))
  | Complex _, Type.Matrix u -> Matrix (Matrix.create 1 1 (upcast v u))

  (* Vector *)
  | Vector _, Type.Vector u when u=t -> v
  | Vector x, Type.Vector u          -> Vector (Vector.map (fun e -> upcast e u) x)
  | Vector x, Type.Matrix u          ->
      begin
        match type_of_t x.(0) with
        | Type.Vector y ->
            let aux = function
              | Vector x -> x
              | _        -> assert false in
            let xx = Array.map aux x in
            Matrix (Matrix.map (fun e -> upcast e u) xx)
        | _ -> 
            Matrix (Vector.map (fun e -> [| upcast e u |]) x)
      end

  (* Matrix *)
  | Matrix _, Type.Matrix u when u=t -> v
  | Matrix x, Type.Matrix u          -> Matrix (Matrix.map (fun e -> upcast e u) x)

  | _ -> Error.uncastable (type_of_t v) t

let try_upcast fn fn_str x y =
  let tx = type_of_t x in
  let ty = type_of_t y in
  if Type.upcastable tx ty then
    fn (upcast x ty) y
  else if Type.upcastable ty tx then
    fn x (upcast y tx)
  else
    Error.unsupported fn_str (type_of_t x) (type_of_t y)

let rec add x y = match (x, y) with
  | Int     x, Int     y -> Int (x+y)
  | Int32   x, Int32   y -> Int32 (Int32.add x y)
  | Int64   x, Int64   y -> Int64 (Int64.add x y)
  | Float   x, Float   y -> Float (x +. y)
  | Complex x, Complex y -> Complex (Complex.add x y)
  | Vector  x, Vector  y ->
      Vector.check_size x y;
      Vector (Vector.map2 add x y)
  | Matrix  x, Matrix  y ->
      Matrix.check_size x y;
      Matrix (Matrix.map2 add x y)
  | _        , _         -> try_upcast add "add" x y

let rec mul x y = match (x, y) with
  | Int     x, Int     y -> Int (x * y)
  | Int32   x, Int32   y -> Int32 (Int32.mul x y)
  | Int64   x, Int64   y -> Int64 (Int64.mul x y)
  | Float   x, Float   y -> Float (x *. y)
  | Complex x, Complex y -> Complex (Complex.add x y)
  | Vector  x, Vector  y ->
      Vector.check_size x y;
      Vector (Vector.map2 mul x y)
  | Matrix  x, Matrix  y  -> Matrix (Matrix.mul add mul x y)
  | _        , _          -> try_upcast mul "mul" x y

let shape x =
  let rec aux = function
    | Vector x -> Array.length x :: aux x.(0)
    | Matrix x -> Array.length x :: Array.length x.(0) :: aux x.(0).(0)
    | _        -> [] in
  match x with
    | Vector x -> Array.length x :: aux x.(0)
    | Matrix x -> Array.length x :: Array.length x.(0) :: aux x.(0).(0)
    | _        -> Error.no_attribute (type_of_t x) "shape"

          
      
