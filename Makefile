CC = ocamlc

MLFILES = \
  point.ml \
  triangle.ml \
  circle.ml \
  vector.ml \
  rectangle.ml


CMOFILES = $(MLFILES:%.ml=%.cmo)

all: $(CMOFILES)
	$(CC) -a -o geom.cma $(CMOFILES)

%.cmo: %.ml
	$(CC) -c $<

clean:
	rm -f a.out *.cma *.cmo *.cmi *~
