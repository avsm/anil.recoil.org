OCAMLMAKEFILE = ../OCamlMakefile

SOURCES = bib.ml fs.ml config.ml db.ml pages.ml papers.ml dispatch.ml server.ml 
RESULT = server
PACKS += orm cohttpserver htmlgen json-tc.syntax orm.syntax
DEBUG = yes
PREDS = camlp4o
ANNOTATE = yes

all: nc
	@ :

depend:
	ln -nsf ../bibtex/bib.ml
	ln -nfs ../fs/fs.ml

-include $(OCAMLMAKEFILE)
