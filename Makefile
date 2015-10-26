OCB_FLAGS = -use-ocamlfind -tag bin_annot -I src
OCB = 		ocamlbuild $(OCB_FLAGS)
OCBTEST = $(OCB) -package graphics -I tests

all: 		native byte # profile debug

clean:
				$(OCB) -clean

native:
				$(OCB) geom.cmxa

byte:
				$(OCB) geom.cma

test:   native byte
				@for file in tests/t_*.ml ; do \
					 $(OCBTEST) $$(basename $$file | cut -f 1 -d '.').byte ; \
				done

doc:
				ocamlbuild geom.docdir/index.html


.PHONY: 	all clean test
