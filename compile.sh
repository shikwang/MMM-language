#!/bin/bash
for var in "$@"
do  
    ./mmm.native $var >> $var.ir;
    llc $var.ir;
    clang++ `pkg-config --cflags opencv` `pkg-config --libs opencv` $var.ir.s io.cpp -o main;
    rm $var.ir;
    rm $var.ir.s
done

    
