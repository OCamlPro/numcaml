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
  | No_attribute of Type.t * string         (* 'int' has no attribute 'shape' *)
  | Not_aligned                             (* objects are not aligned *)
  | Unsupported of string * Type.t * Type.t (* unsupported operand type(s) for +: 'int' and 'str' *)
  | Uncastable of Type.t * Type.t

let sf = Printf.sprintf

let fail = function
  | No_attribute (t, a) -> failwith (sf "'%s' has no attribute '%s'" (Type.to_string t) a)
  | Not_aligned         -> failwith "objects are not aligned"
  | Unsupported (s,t,u) ->
    let msg = 
      sf "unsupported operand type(s) for %s: '%s' and '%s'"
        s (Type.to_string t) (Type.to_string u) in
      failwith msg
  | Uncastable (t1,t2)  ->
      failwith (sf "'%s' cannot be upcast to '%s'" (Type.to_string t1) (Type.to_string t2))

let no_attribute t a = fail (No_attribute (t, a))
let not_aligned () = fail Not_aligned
let unsupported s t u = fail (Unsupported (s,t,u))
let uncastable t1 t2 = fail (Uncastable (t1,t2))
