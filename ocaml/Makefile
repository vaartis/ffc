OCB_FLAGS = -use-ocamlfind -use-menhir -I src
OCB = ocamlbuild ${OCB_FLAGS}

all:
	${OCB} main.native

clean:
	${OCB} -clean
