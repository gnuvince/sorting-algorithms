all:
	ocamlbuild -lib unix bench.native

clean:
	ocamlbuild -clean bench.native
