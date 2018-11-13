func matrix height(matrix m){  /* should fail because cannot have function named height */
    m = m * m;
    return m;
}

func void main(){
    matrix m = [1.1,1.0;2.2,2.3];
    height(m);
}