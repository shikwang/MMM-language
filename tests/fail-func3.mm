func matrix Foo(){
    matrix m = [1.1,1.0,1.2;2.2,2.3,2.4;3.1,3.2,3.3];
    matrix a = eig(m);
    return a;
}

func void main(){
    matrix a = Bar();   /* this should fail because unrecognized func */
}