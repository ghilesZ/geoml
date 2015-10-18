CC = ocamlc
GEOMLDIR = /home/ghiles/geoml/lib
OCAMLINC = -I $(GEOMLDIR)
WITHGRAPHICS = graphics.cma
WITHGEOM = geom.cma $(OCAMLINC)

all:
	ocamlc $(WITHGRAPHICS) $(WITHGEOM) drawing.ml t_01.ml -o t_01
	ocamlc $(WITHGRAPHICS) $(WITHGEOM) drawing.ml t_02.ml -o t_02
clean:
	rm -f *.cm[io] *~
