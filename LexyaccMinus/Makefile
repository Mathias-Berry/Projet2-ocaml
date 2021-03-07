all: 
	ocamlbuild -yaccflag -v -lib unix main.native #on dit de fabriquer main.native
	mv main.native fouine

byte: 
	ocamlbuild -yaccflag -v main.byte
	mv main.byte fouine

clean: 
	ocamlbuild -clean
