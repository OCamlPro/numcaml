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

open Camlp4

module Id : Sig.Id = 
  struct 
    let name = "numcaml" 
    let version = "0.1" 
  end 

module Make (Syntax : Sig.Camlp4Syntax) = 
  struct 
    open Sig
    include Syntax 

    let map = object
      inherit Ast.map as super
      method expr e =
	let _loc = Ast.loc_of_expr e in
	match super#expr e with

	  | <:expr< $int:i$ >>   -> <:expr< Math.Int $int:i$ >>
	  | <:expr< $int32:i$ >> -> <:expr< Math.Int32 $int32:i$ >>
	  | <:expr< $int64:i$ >> -> <:expr< Math.Int64 $int64:i$ >>
	  | <:expr< $flo:i$ >>   -> <:expr< Math.Float $flo:i$ >>

	  | <:expr< $e1$ + $e2$ >> -> <:expr< Math.add $e1$ $e2$ >>
	  | <:expr< $e1$ * $e2$ >> -> <:expr< Math.mul $e1$ $e2$ >>

          | <:expr< ( $e1$ : int ) >>   -> <:expr< Math.Int $e1$ >>
          | <:expr< ( $e1$ : int32 ) >> -> <:expr< Math.Int32 $e1$ >>
          | <:expr< ( $e1$ : int64 ) >> -> <:expr< Math.Int64 $e1$ >>
          | <:expr< ( $e1$ : float ) >> -> <:expr< Math.Float $e1$ >>

          | <:expr< ( $e1$ : vector $lid:t$) >> ->
              let constr = String.capitalize t in
              <:expr< Math.Vector (Array.map (fun e -> Math.$uid:constr$ e) $e1$) >>
          | <:expr< ( $e1$ : matrix $lid:t$) >> -> <:expr< Math.Matrix $e1$ >>

	  | e                      -> e
    end
  
    EXTEND Gram
      expr: LEVEL "top"
      [ 
	[ "$"; "("; e = expr; ")" -> map#expr e ]
      ];
    END 
  end 
  
module M = Register.OCamlSyntaxExtension(Id)(Make) 

