OCB_FLAGS = -use-ocamlfind -use-menhir -Is syntax,src
OCB = ocamlbuild ${OCB_FLAGS}

all:
	${OCB} main.native

test:
	${OCB} -I tests -pkg oUnit run_tests.native

clean:
	${OCB} -clean
