P4_INC = $(shell ocamlc -where)/camlp4
PP = camlp4o -I $(P4_INC) \
	-parser Camlp4GrammarParser \
	-parser Camlp4QuotationCommon \
	-parser Camlp4OcamlRevisedQuotationExpander

.PHONY: all clean
all:
	ocamlc -I $(P4_INC) -pp "$(PP)" -c pa_numcaml.ml

clean:
	rm -f *.cmo *.cmi