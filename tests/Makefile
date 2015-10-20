CC = ocamlc
GEOMLDIR = /home/ghiles/geoml/lib
OCAMLINC = -I $(GEOMLDIR)
WITHGRAPHICS = graphics.cma
WITHGEOM = geom.cma $(OCAMLINC)
UTILS = drawing.ml tester.ml

all:
	ocamlc $(WITHGRAPHICS) $(WITHGEOM) $(UTILS) t_01.ml -o t_01
	ocamlc $(WITHGRAPHICS) $(WITHGEOM) $(UTILS) t_02.ml -o t_02
	ocamlc $(WITHGRAPHICS) $(WITHGEOM) $(UTILS) t_03.ml -o t_03

clean:
	rm -f *.cm[io] *~
