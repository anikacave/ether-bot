MODULES=ether_csv ether_scan_processing ether_scan_query main csv_writer wealth analyze ascii_graph get_info indicators wealth_bot wealth_ui
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
CSV_WRITER=csv_writer.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

run:
	$(OCAMLBUILD) $(MAIN) -tag 'debug' $(CSV_WRITER) && OCAMLRUNPARAM=b ./$(MAIN)

run_csv_writer:
	$(OCAMLBUILD) $(CSV_WRITER) && OCAMLRUNPARAM=b ./$(CSV_WRITER)

zip:
	zip bot.zip *.ml* *.txt  _tags .merlin .ocamlformat .ocamlinit  Makefile	
	
docs: docs-public docs-private
	
docs-public: build
	mkdir -p _doc.public
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal,csv,lwt,stringext,cohttp,cohttp-lwt-unix,  \
		-html -stars -d _doc.public $(MLIS)

docs-private: build
	mkdir -p _doc.private
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal,csv,lwt,stringext,cohttp,cohttp-lwt-unix, \
		-html -stars -d _doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf _doc.public _doc.private bot.zip
