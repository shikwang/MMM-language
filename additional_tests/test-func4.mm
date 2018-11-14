func matrix Foo(){
    matrix m = [1.1,1.0,1.2;2.2,2.3,2.4;3.1,3.2,3.3];
    matrix a = m[:][1:2];
    return a;
}

func void main(){
    matrix a = Foo();   /* this should success */
    print("yes");
}