FILES = \
	type \
	error \
	vector \
	matrix \
	math

CMX = $(FILES:%=numcaml/%.cmx)

%.cmx: %.ml
	ocamlopt -c -for-pack Numcaml -I numcaml $*.ml

.PHONY: all clean
all: $(CMX)
	ocamlopt -pack -o numcaml.cmx $(CMX)

clean:
	rm -f *.cmx *.o *.cmi
	rm -f numcaml/*.cmx numcaml/*.o numcaml/*.cmi
