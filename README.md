Sorting Algorithms
==================

Implementation of various sorting algorithms in OCaml.

Compiling
---------

$ ocamlbuild -lib unix bench.native


Running
-------

Run bench.native.  Use the --help option to view command line options


Future work
-----------

* Add command-line option to select different types of elements (floats, strings)
* Implement other algorithms (heapsort, timsort, etc.)
* Add a test suite to make sure that all sorting algorithms are correct
