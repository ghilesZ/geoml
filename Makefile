.PHONY: default build install uninstall test clean

default: build doc

build:
	dune build

test:
	@dune runtest --force -j1 --no-buffer

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean

doc:
	dune build @doc
	mkdir -p "docs"
	cp -fr _build/default/_doc/_html/* docs/
