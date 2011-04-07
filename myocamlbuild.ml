(* We need that file to compile the tests without having to install the libs *)

open Ocamlbuild_plugin

let pa_numcaml = "syntax/pa_numcaml.cmo"
let pp_camlp4  = S [
  A "-pp";
  A ("camlp4o "^pa_numcaml)
]
let lib = S [
  A "-I";
  A "lib";
]

let byte = A "numcaml.cmo"
let native = A "numcaml.cmx"
 
let _ = dispatch begin function
  | After_rules ->
    flag ["ocaml"; "compile"; "tests"] pp_camlp4;
    flag ["ocaml"; "compile"; "tests"] lib;

    flag ["ocaml"; "ocamldep"; "tests"] pp_camlp4;

    flag ["ocaml"; "link"; "tests"           ] lib;
    flag ["ocaml"; "link"; "tests"; "byte"   ] byte;
    flag ["ocaml"; "link"; "tests"; "native" ] native;

  | _ -> ()
end
