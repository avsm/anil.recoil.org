OCAMLMAKEFILE = ../OCamlMakefile

SOURCES = files.ml templates.ml
LIB_PACK_NAME = static
RESULT = static
TRASH = files.ml templates.ml
OCAMLC=ocamlc.opt
OCAMLOPT=ocamlopt.opt
OCAMLDEP=ocamldep.opt

all: depend panc static.cmxa
	@ :

.PHONY: depend all
depend: files.ml templates.ml
	@ :

STATIC_FILES=../files
STATIC_TEMPLATES=../templates
files.ml: 
	mlcrunch $(STATIC_FILES) > $@
templates.ml: $(wildcard $(STATIC_TEMPLATE))
	mlcrunch $(STATIC_TEMPLATES) > $@

-include $(OCAMLMAKEFILE)
