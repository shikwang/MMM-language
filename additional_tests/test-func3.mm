func matrix Foo(){
    matrix m = [1.1,1.0;2.2,2.3];
    matrix a = m[:][0];
    print("aa");
    return a;
}

func void main(){
    matrix a = Foo();     /* this should success */
}