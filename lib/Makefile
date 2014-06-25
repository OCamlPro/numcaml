FILES = \
	type \
	error \
	vector \
	matrix \
	math

CMX = $(FILES:%=numcaml/%.cmx)
CMO = $(FILES:%=numcaml/%.cmo)

%.cmx: %.ml
	ocamlopt -c -for-pack Numcaml -I numcaml $*.ml

%.cmo: %.ml
	ocamlc -c -for-pack Numcaml -I numcaml $*.ml

.PHONY: all clean
all: $(CMX) $(CMO)
	ocamlopt -pack -o numcaml.cmx $(CMX)
	ocamlc -pack -o numcaml.cmo $(CMO)

clean:
	rm -f *.cmx *.o *.cmi *.cmo
	rm -f numcaml/*.cmx numcaml/*.o numcaml/*.cmi numcaml/*.cmo
