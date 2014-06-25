.PHONY: all clean lib syntax tests

all: lib syntax
	@

lib:
	$(MAKE) -C lib

syntax:
	$(MAKE) -C syntax

tests: all
	$(MAKE) -C tests

install:
	ocamlfind install numcaml \
		META \
		lib/numcaml.cmi lib/numcaml.cmo lib/numcaml.o lib/numcaml.cmx \
		syntax/pa_numcaml.cmo

uninstall:
	ocamlfind remove numcaml

clean:
	$(MAKE) -C lib clean
	$(MAKE) -C syntax clean
	$(MAKE) -C tests clean
	- find . -name "*~" | xargs rm
