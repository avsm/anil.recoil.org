OCAMLMAKEFILE = OCamlMakefile

SOURCES = static_files.ml static_templates.ml config.ml pages.ml dispatch.ml server.ml 
RESULT = server
PACKS += orm cohttpserver
PACKS += lwt.syntax
DEBUG = yes
ANNOTATE = yes
REALTRASH = static_files.ml static_templates.ml

.PHONY: depend all
depend: static_files.ml static_templates.ml
	@ :

all: nc
	@ :

distclean: clean
	rm -f $(REALTRASH)

STATIC_FILES=../files
STATIC_TEMPLATES=../templates
static_files.ml:
	mlcrunch $(STATIC_FILES) > $@
static_templates.ml:
	mlcrunch -ext inc $(STATIC_TEMPLATES) > $@

-include $(OCAMLMAKEFILE)
