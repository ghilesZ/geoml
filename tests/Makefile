CC = ocamlc
GEOMLDIR= /home/ghiles/geoml/lib
OCAMLINC = -I $(GEOMLDIR)
WITHGRAPHICS = graphics.cma
WITHGEOM =  geom.cma $(OCAMLINC)

all:
	ocamlc $(WITHGRAPHICS) $(WITHGEOM) t_01.ml -o t_01

clean:
	rm -f *.cm[io] *~
