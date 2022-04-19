UNITS=ast_factory authors eval interp parse repl
MLS_WITHOUT_MLIS=ast
MLS=$(UNITS:=.ml) $(MLS_WITHOUT_MLIS:=.ml)
OBJECTS=$(UNITS:=.cmo) $(MLS_WITHOUT_MLIS:=.cmo) parser.cmo
MLIS=$(UNITS:=.mli)
TEST=test.byte
REPL=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind -plugin-tag \
	'package(bisect_ppx-ocamlbuild)'
PKGS=ounit2,str

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag debug $(TEST) && ./$(TEST)

bisect-test:
	BISECT_COVERAGE=YES $(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

bisect: clean bisect-test
	bisect-ppx-report html

repl:
	$(OCAMLBUILD) -tag 'debug' $(REPL) && OCAMLRUNPARAM=b rlwrap ./$(REPL)

check:
	@bash check.sh
	
finalcheck:
	@bash check.sh final

zip:
	zip jocalf.zip *.ml* *.sh _tags .merlin \
		.ocamlformat .ocamlinit LICENSE Makefile

docs: docs-public docs-private

docs-public: build
	mkdir -p _doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d _doc.public $(MLIS)

docs-private: build
	mkdir -p _doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d _doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf jocalf.zip _doc.public _doc.private _coverage bisect*.coverage
