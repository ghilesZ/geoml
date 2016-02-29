OCB_FLAGS = -use-ocamlfind
OCB = 		ocamlbuild $(OCB_FLAGS) -I src
OCBTEST = ocamlbuild $(OCB_FLAGS) -package graphics -lflags '-I .'

TESTS_SRCS = $(wildcard tests/*_t.ml)

TESTS_BYTE = $(patsubst %.ml,tests/%_t.byte,$(TESTS_SRCS))
TESTS_NATIVE = $(patsubst %.ml,tests/%_t.native,$(TESTS_SRCS))

all: 		native byte # profile debug

clean:
				$(OCB) -clean

native:
				$(OCB) geom.cmxa

byte:
				$(OCB) geom.cma

tests: $(TESTS_BYTE)

tests.native: $(TESTS_NATIVE)

tests/%_t.native: %.ml
				$(OCBTEST) $(basename $<).native

tests/%_t.byte: %.ml
				$(OCBTEST) $(basename $<).byte


doc:
				ocamlbuild geom.docdir/index.html


.PHONY: 	all clean tests
