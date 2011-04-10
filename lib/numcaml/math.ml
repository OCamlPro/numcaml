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

module T = Type

type t =
  | Int     of int
  | Int32   of int32
  | Int64   of int64
  | Float   of float
  | Complex of Complex.t
  | Vector  of t Vector.t
  | Matrix  of t Matrix.t

let rec type_of_t = function
  | Int     _ -> T.Int
  | Int32   _ -> T.Int32
  | Int64   _ -> T.Int64
  | Float   _ -> T.Float
  | Complex _ -> T.Complex
  | Vector  x ->
      T.Vector {
        T.v_t = type_of_t x.(0);
             v_n = Array.length x;
      }
  | Matrix  x ->
      T.Matrix { 
        T.m_t = type_of_t x.(0).(0);
             m_n = Array.length x;
             m_m = Array.length x.(0);
      }

let rec upcast v t = match (v, t) with

  (* Int *)
  | Int _, T.Int      -> v
  | Int x, T.Int32    -> Int32 (Int32.of_int x)
  | Int x, T.Int64    -> Int64 (Int64.of_int x)
  | Int x, T.Float    -> Float (float x)
  | Int x, T.Complex  -> Complex { Complex.re = float x; im = 0. }

  (* Int32 *)
  | Int32 _, T.Int32    -> v
  | Int32 x, T.Int64    -> Int64 (Int64.of_int32 x)
  | Int32 x, T.Float    -> Float (Int32.to_float x)
  | Int32 x, T.Complex  -> Complex { Complex.re = Int32.to_float x; im = 0. }

  (* Int64 *)
  | Int64 _, T.Int64    -> v
  | Int64 x, T.Float    -> Float (Int64.to_float x)
  | Int64 x, T.Complex  -> Complex { Complex.re = Int64.to_float x; im = 0. }

  (* Float *)
  | Float _, T.Float    -> v
  | Float x, T.Complex  -> Complex { Complex.re = x; im = 0. }

  (* Complex *)
  | Complex x, T.Complex  -> v

  (* Vector *)
  | Vector _, T.Vector u when u.T.v_t=t -> v
  | Vector x, T.Vector u -> Vector (Vector.map (fun e -> upcast e u.T.v_t) x)
  | Vector x, T.Matrix u ->
      begin
        match type_of_t x.(0) with
        | T.Vector y ->
            let aux = function
              | Vector x -> x
              | _        -> assert false in
            let xx = Array.map aux x in
            Matrix (Matrix.map (fun e -> upcast e u.T.m_t) xx)
        | _ -> 
            Matrix (Vector.map (fun e -> [| upcast e u.T.m_t |]) x)
      end

  (* Matrix *)
  | Matrix _, T.Matrix u when u.T.m_t=t -> v
  | Matrix x, T.Matrix u -> Matrix (Matrix.map (fun e -> upcast e u.T.m_t) x)

  | _, T.Vector u -> Vector (Vector.make u.T.v_n (upcast v u.T.v_t))
  | _, T.Matrix u -> Matrix (Matrix.make u.T.m_n u.T.m_m (upcast v u.T.m_t))

  | _ -> Error.uncastable (type_of_t v) t

let upcast_apply fn x y =
  let tx = type_of_t x in
  let ty = type_of_t y in
  let t = T.top tx ty in
  fn (upcast x t) (upcast y t)

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
  | _        , _         -> upcast_apply add x y

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
  | _        , _          -> upcast_apply mul x y

let shape x =
  let rec aux = function
    | Vector x -> Array.length x :: aux x.(0)
    | Matrix x -> Array.length x :: Array.length x.(0) :: aux x.(0).(0)
    | _        -> [] in
  match x with
    | Vector x -> Array.length x :: aux x.(0)
    | Matrix x -> Array.length x :: Array.length x.(0) :: aux x.(0).(0)
    | _        -> Error.no_attribute (type_of_t x) "shape"

let rec zeros = function
  | []   -> Vector [| Int 0 |]
  | [i]  -> Vector (Array.init i (fun _ -> Int 0))
  | i::t -> Vector (Array.init i (fun _ -> zeros t))

let rec ones = function
  | []   -> Vector [| Int 1 |]
  | [i]  -> Vector (Array.init i (fun _ -> Int 1))
  | i::t -> Vector (Array.init i (fun _ -> ones t))

let id n =
  let m = Matrix.make n n 0 in
  for i = 0 to n-1 do
    m.(i).(i) <- 1
  done
