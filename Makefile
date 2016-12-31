#  This file is part of the ppx_tools package.  It is released
#  under the terms of the MIT license (see LICENSE file).
#  Copyright 2013  Alain Frisch and LexiFi

include $(shell ocamlc -where)/Makefile.config

PACKAGE = ocaml-migrate-parsetree
VERSION = 1.0
# Don't forget to change META file as well

OCAMLC = ocamlc -bin-annot
OCAMLOPT = ocamlopt
COMPFLAGS = -w +A-4-17-44-45-105 -I +compiler-libs -safe-string

.PHONY: all
all: migrate_parsetree.cma

.PHONY: clean
clean:
	rm -f *.cm* *~ *.o *.obj *.a *.lib *.tar.gz *.cmxs *.cmt *.cmti
	rm -f OCamlFrontend*.ml OCamlFrontend*.mli

# Default rules

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(COMPFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(COMPFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(COMPFLAGS) -c $<

# Install/uninstall

targets = $(1).mli $(1).cmi $(1).cmt $(1).cmti $(wildcard $(1).cmx)
INSTALL = META \
   migrate_parsetree.cma \
   $(wildcard migrate_parsetree.cmxa migrate_parsetree$(EXT_LIB)) \
	 $(MIGRATE_PARSETREE:.cmo=.cmi) $(OCAML_FRONTENDS:.mli=.cmi)

.PHONY: reinstall install uninstall

install:
	ocamlfind install $(PACKAGE) $(INSTALL)

uninstall:
	ocamlfind remove $(PACKAGE)

reinstall:
	$(MAKE) uninstall
	$(MAKE) install

# Snapshoted versions of Parsetree (and related modules) for different versions of OCaml

OCAMLCURRENT=$(shell $(OCAMLC) -config | ./frontend_version.sh)
OCAML_FRONTENDS=OCamlFrontend404.mli OCamlFrontend403.mli OCamlFrontend402.mli

$(OCAML_FRONTENDS):
	./build_frontends.sh $(OCAMLCURRENT)

## ./gencopy -I . -map OCamlFrontend403:OCamlFrontend404 OCamlFrontend403.Parsetree.expression > migrate_parsetree_403_404.ml
## ./gencopy -I . -map OCamlFrontend404:OCamlFrontend403 OCamlFrontend404.Parsetree.expression > migrate_parsetree_404_403.ml
## ./gencopy -I . -map OCamlFrontend402:OCamlFrontend403 OCamlFrontend402.Parsetree.expression > migrate_parsetree_402_403.ml
## ./gencopy -I . -map OCamlFrontend403:OCamlFrontend402 OCamlFrontend403.Parsetree.expression > migrate_parsetree_403_402.ml

MIGRATE_PARSETREE = \
	migrate_parsetree_def.cmo \
	migrate_parsetree_403_404.cmo \
	migrate_parsetree_404_403.cmo \
	migrate_parsetree_402_403.cmo \
	migrate_parsetree_403_402.cmo \
	migrate_parsetree_reader.cmo

migrate_parsetree.cma: $(MIGRATE_PARSETREE)
	$(OCAMLC) -a -o migrate_parsetree.cma $(MIGRATE_PARSETREE)

migrate_parsetree.cmxa: $(MIGRATE_PARSETREE:.cmo=.cmx)
	$(OCAMLOPT) -a -o migrate_parsetree.cmxa $(MIGRATE_PARSETREE:.cmo=.cmx)

.PHONY: depend
depend: $(OCAML_FRONTENDS)
	touch ast_lifter.ml
	ocamldep *.ml *.mli > .depend
	dos2unix .depend
-include .depend
