# "make test" Compiles everything and runs the regression tests

.PHONY : test
test : all testall.sh
	./testall.sh

# "make all" builds the executable as well as the "printbig" library designed
# to test linking external code

.PHONY : all
all : mmm.native

# "make mmm.native" compiles the compiler
#
# The _tags file controls the operation of ocamlbuild, e.g., by including
# packages, enabling warnings
#

mmm.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind mmm.native

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf testall.log ocamlllvm *.diff

# Building the tarball

TESTS = \
  for1 func1 func2 func3 func4 string1

FAILS = \
  assign1 assign2 assign3 assign4 \
  for1 func1 func2 func3 func4 func5 \
  if1 matrix1

TESTFILES = $(TESTS:%=test-%.mmm) $(TESTS:%=test-%.out) \
	    $(FAILS:%=fail-%.mmm) $(FAILS:%=fail-%.err)

TARFILES = ast.ml sast.ml codegen.ml Makefile _tags mmm.ml parser.mly \
	README scanner.mll semant.ml testall.sh \
	$(TESTFILES:%=tests/%) 

mmm.tar.gz : $(TARFILES)
	cd .. && tar czf MMM-language/mmm.tar.gz \
		$(TARFILES:%=mmm/%)