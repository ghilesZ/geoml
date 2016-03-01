OCP = ocp-build



all: tests

init:
	test -s _obuild || ocp-build init

build: init
	@@ocp-build build 2>&1 >/dev/null | grep -v "Warning:"

tests: init
	@@ocp-build tests 2>&1 >/dev/null | grep -v "Warning:"
	@@test -s runtest.byte || ln -s _obuild/runtest/runtest.byte runtest.byte

clean:
	@rm -rf _obuild


# OCB_FLAGS = -use-ocamlfind
# OCB = 		ocamlbuild $(OCB_FLAGS) -I src
# OCBTEST = ocamlbuild $(OCB_FLAGS) -package graphics -lflags '-I .'

# TESTS_SRCS = $(wildcard tests/*_t.ml)
# TESTS_BYTE = $(patsubst %.ml,tests/%_t.byte,$(TESTS_SRCS))
# TESTS_NATIVE = $(patsubst %.ml,tests/%_t.native,$(TESTS_SRCS))



# clean:
# 				$(OCB) -clean

# native:
# 				$(OCB) geom.cmxa

# byte:
# 				$(OCB) geom.cma

# tests: $(TESTS_BYTE)

# tests.native: $(TESTS_NATIVE)

# tests/%_t.native: %.ml
# 				$(OCBTEST) $(basename $<).native

# tests/%_t.byte: %.ml
# 				$(OCBTEST) $(basename $<).byte

# ocp:
# 	ocp-build init
# 	ocp-build build

# doc:
# 				ocamlbuild geom.docdir/index.html


# .PHONY: 	all clean tests
