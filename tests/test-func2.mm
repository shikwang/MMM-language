func float Foo(){
    matrix m = [1.1,1.0;2.2,2.3];
    float a = m[0][0];
    return a;
}

func void main(){
    float a = Foo();     /* this should success */
    print("ggg);
}