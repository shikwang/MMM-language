cat $1 | ./mmm.native > source.llvm; llc source.llvm -o source.s; gcc source.s -o main; rm source.s source.llvm
