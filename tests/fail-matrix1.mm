func matrix Foo(){
    matrix m = [1.1,1.0,1.2;2.2,2.3,2.4;3.1,3.2,3.3];
    int a = 123;
    int a = mean(123);  /* this should fail because mean input a int */
    return a;
}

func void main(){
    matrix a = Foo();
}