.PHONY: all clean lib syntax tests

all: lib syntax
	@

lib:
	$(MAKE) -C lib

syntax:
	$(MAKE) -C syntax

tests:
	$(MAKE) -C tests

clean:
	$(MAKE) -C lib clean
	$(MAKE) -C syntax clean
	$(MAKE) -C tests clean
