all:
	ocamlbuild -lib unix bench.native

test:
	ocamlbuild test.native
	./test.native

clean:
	ocamlbuild -clean bench.native
