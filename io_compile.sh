#!/bin/bash
for var in "$@"
do  
    rm $var.ir;
    ./mmm.native $var >> $var.ir;
    llc $var.ir;
    clang++ `pkg-config --cflags opencv` `pkg-config --libs opencv` $var.ir.s io.cpp -o main
done

    
