# "make all" builds the executable

.PHONY : all
all : panamx.native matrix.o

# "make panamx.native" compiles the compiler
#
# The _tags file controls the operation of ocamlbuild, e.g., by including
# packages, enabling warnings
#
# See https://github.com/ocaml/ocamlbuild/blob/master/manual/manual.adoc

panamx.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind panamx.native

# "make test" Compiles everything and runs the regression tests

.PHONY : test
test : all testall.sh
	./testall.sh

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf testall.log ocamlllvm *.diff matrix.o *.ll *.out *.exe *.s *.err

# Link the matrix.c to llvm

matrix : matrix.c
	cc -o matrix -DBUILD_TEST matrix.c
