MAKESRC=$(MAKE) -C src
MAKETESTS=$(MAKE) -C tests
MAKEDOC=$(MAKE) doc -C src

all: lib tests

build: 
	mkdir lib
	mkdir bin

lib:
	$(MAKESRC)
	mv src/geom.cma lib/
	cp src/*.cmi lib/

tests:
	$(MAKETESTS)
	mv tests/t_01 bin/
	mv tests/t_02 bin/

doc:
	$(MAKESRC)
	$(MAKEDOC) 

clean:
	$(MAKE) clean -C src
	$(MAKE) clean -C tests
	rm -f lib/* *~ doc/*.html doc/*.css bin/*

.PHONY: lib tests doc
