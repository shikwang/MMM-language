func matrix Foo(){
    matrix m = [1.1,1.0;2.2,2.3];
    m = height(m);
    return m;
}

func void main(){
    matrix m = Foo();    /* this should success */
    print("aaa");
}