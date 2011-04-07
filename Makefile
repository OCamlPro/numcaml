.PHONY: all clean lib syntax tests

all: lib syntax
	@

lib: lib/*/*.ml
	ocamlbuild -use-ocamlfind lib/numcaml.cmx

syntax: syntax/*.ml
	ocamlbuild -use-ocamlfind syntax/pa_numcaml.cmo

clean:
	ocamlbuild -clean

tests: tests/*.ml syntax/*.ml lib/*/*.ml
	ocamlbuild -use-ocamlfind tests/suite.native -- -verbose

tests/%_exp.ml: syntax
	camlp4o -printer o $(shell ocamlfind query camlp4 -i-format) \
		_build/syntax/pa_numcaml.cmo tests/$*.ml
