CC = ocamlc

MLFILES = \
  point.ml \
  line.ml \
  segment.ml \
  triangle.ml \
  circle.ml \
  vector.ml \
  rectangle.ml


CMOFILES = $(MLFILES:%.ml=%.cmo)

all: $(CMOFILES)
	$(CC) -a -o geom.cma $(CMOFILES)

doc:
	ocamldoc -m A -html -d ../doc $(MLFILES)

%.cmo: %.ml
	$(CC) -c $<

clean:
	rm -f a.out *.cma *.cmo *.cmi *~ ../doc/*.html ../doc/*.css
