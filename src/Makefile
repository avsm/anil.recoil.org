OCAMLMAKEFILE = OCamlMakefile

SOURCES = static_files.ml static_templates.ml config.ml pages.ml dispatch.ml server.ml 
RESULT = server
PACKS += orm cohttpserver htmlgen
PACKS += lwt.syntax
DEBUG = yes
ANNOTATE = yes
REALTRASH = static_files.ml static_templates.ml

all: nc
	@ :

.PHONY: depend all
depend: static_files.ml static_templates.ml
	@ :

distclean: clean
	rm -f $(REALTRASH)

STATIC_FILES=../files
STATIC_TEMPLATES=../templates
static_files.ml: 
	mlcrunch $(STATIC_FILES) > $@
static_templates.ml: $(wildcard $(STATIC_TEMPLATE))
	mlcrunch $(STATIC_TEMPLATES) > $@

-include $(OCAMLMAKEFILE)
