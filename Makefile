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

test:
				$(OCBTEST) t_01.byte
				$(OCBTEST) t_02.byte
				$(OCBTEST) t_03.byte


.PHONY: 	all clean test
