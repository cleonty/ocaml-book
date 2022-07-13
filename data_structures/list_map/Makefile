MODULES=maps
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=maps_test.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind -tag 'debug' $(TEST) && ./$(TEST) -runner sequential 
PKGS=ounit2

default: 
	$(OCAMLBUILD)

