################################# ocp-build ###############################
# If you want to build using ocp-build instead of dune, uncomment these lines
#	all: tests

# building using ocp-build
# OCP = ocp-build

# init:
# 	test -s _obuild || ocp-build init

# build: init
# 	@@ocp-build build 2>&1 >/dev/null | grep -v "Warning:"

# tests: init
# 	@@ocp-build tests 2>&1 >/dev/null | grep -v "Warning:"
# 	@@test -s runtest.byte || ln -s _obuild/runtest/runtest.byte runtest.byte

# clean:
# 	@rm -rf _obuild
######################### end of ocp-build ################################


############################# ocamlbuild ##################################

# If you want to build using ocamlbuild instead of dune, uncomment these lines
# OCB_FLAGS = -use-ocamlfind
# OCB       = ocamlbuild $(OCB_FLAGS) -I src
# OCBTEST   = ocamlbuild $(OCB_FLAGS) -package graphics -lflags '-I .'

# TESTS_SRCS   = $(wildcard tests/runtest.ml)
# TESTS_BYTE   = $(patsubst %.ml,tests/%_t.byte,$(TESTS_SRCS))
# TESTS_NATIVE = $(patsubst %.ml,tests/%_t.native,$(TESTS_SRCS))

#	all: tests

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

# doc:
# 				ocamlbuild geom.docdir/index.html


# .PHONY: 	all clean tests
########################## end of ocamlbuild ##############################

########################## Frontend to dune ##############################
.PHONY: default build install uninstall test clean

default: build doc

build:
	dune build

test:
	dune runtest -f

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean

doc:
	dune build @doc
	mkdir -p docs/
	cp -r _build/default/_doc/_html/* docs/
