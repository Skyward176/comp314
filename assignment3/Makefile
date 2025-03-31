# A2 makefile
#
# targets are:
#
# all -- rebuild the project (default)
# clean -- remove all objects and executables

SOURCES = src/ast.ml src/Parser/parser.mli src/Parser/lexer.ml src/Parser/parser.ml src/eval.ml src/assignment3.ml

all: imp

clean:
	rm -f imp
	for X in ./src ./src/Parser; do \
      for Y in cmo cmi output; do \
        rm -f $$X/*.$$Y; \
      done; \
    done

imp: $(SOURCES)
	ocamlc -o imp -g -I src/ -I src/Parser str.cma $(SOURCES)

src/Parser/parser.mli src/Parser/parser.ml: src/Parser/parser.mly
	ocamlyacc -v src/Parser/parser.mly

src/Parser/lexer.ml: src/Parser/lexer.mll src/Parser/parser.ml
	ocamllex src/Parser/lexer.mll
