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
	mkdir -p "docs"
	cp -r _build/default/_doc/_html/geoml/Geoml/ docs/Geoml
