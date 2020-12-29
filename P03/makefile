main: lexer.cmo parser.cmo main.cmo
	ocamlc -o parser lexer.cmo parser.cmo main.cmo

lexer.cmo: lexer.ml
	ocamlc -c lexer.ml

parser.cmo: parser.ml
	ocamlc -c parser.ml

main.cmo: main.ml
	ocamlc -c main.ml

lexer.ml: parser.cmi lexer.mll
	ocamllex lexer.mll

parser.cmi: parser.mli
	ocamlc -c parser.mli

parser.mli: parser.mly
	ocamlyacc parser.mly

parser.ml: parser.mly
	ocamlyacc parser.mly

clean:
	@echo $(DELIM) Cleaning up... $(DELIM)
	@rm -f *.cmo *.cmi *.mli parser parser.ml lexer.ml