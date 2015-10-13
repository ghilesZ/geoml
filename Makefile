MAKESRC=$(MAKE) -C src
MAKETESTS=$(MAKE) -C tests
MAKEDOC=$(MAKE) doc -C src

all: lib tests

lib:
	$(MAKESRC)
	mkdir lib
	mv src/geom.cma lib/
	cp src/*.cmi lib/

tests:
	$(MAKETESTS)
	mv tests/t_01 bin/

doc:
	$(MAKESRC)
	$(MAKEDOC) 

clean:
	$(MAKE) clean -C src
	$(MAKE) clean -C tests
	rm -f lib/* *~ doc/*.html doc/*.css bin/*
	rm -R lib

.PHONY: lib tests doc
