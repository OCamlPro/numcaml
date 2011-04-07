P4_INC = $(shell ocamlc -where)/camlp4

TESTS = \
	tests \
	ints \
	floats \
	vectors \
	matrices \
	suite

CMX = $(TESTS:%=%.cmx)

PP = camlp4o -I $(P4_INC) ../syntax/pa_numcaml.cmo

.PHONY: all rebuild clean
all: tests ../lib/numcaml.cmx ../syntax/pa_numcaml.cmo
	./tests

clean:
	rm -f tests *.cmx *.cmi *.o

rebuild:
	$(MAKE) clean
	$(MAKE) all

tests: $(CMX)
	ocamlopt -I ../lib numcaml.cmx $(CMX) -o tests

%.cmx: %.ml ../syntax/pa_numcaml.cmo
	ocamlopt -pp "$(PP)" -I ../lib -c $*.ml

%_exp.ml: %.ml ../syntax/pa_numcaml.cmo
	camlp4o -printer o -I $(P4_INC) ../syntax/pa_numcaml.cmo $*.ml

