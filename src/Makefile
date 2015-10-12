CC = ocamlc

MLFILES = \
  point.ml \
  line.ml \
  segment.ml \
  vector.ml \
  triangle.ml \
  circle.ml \
  rectangle.ml

CMOFILES = $(MLFILES:%.ml=%.cmo)

all: $(CMOFILES)
	$(CC) -linkall -a $(CMOFILES) -o geom.cma

doc:
	ocamldoc -m A -html -d ../doc $(MLFILES)

%.cmo: %.ml
	$(CC) -c $<

clean:
	rm -f a.out *.cma *.cmo *.cmi *~
