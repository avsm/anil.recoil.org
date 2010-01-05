OCAMLMAKEFILE = ../OCamlMakefile

SOURCES = config.ml papers.ml pages.ml dispatch.ml server.ml 
LIBS = static
INCDIRS = ../crunch
RESULT = server
PACKS += orm cohttpserver htmlgen orm.syntax
DEBUG = yes
PREDS = camlp4o
ANNOTATE = yes

all: nc
	@ :

.PHONY: depend all
depend: 
	$(MAKE) -f Makefile.static all

-include $(OCAMLMAKEFILE)
