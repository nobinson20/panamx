int main() {
    matrix mat;
    mat = <2, 3>;

    mat[0][0] = 1.1;
    mat[0][1] = 2.2;
    mat[0][2] = 3.3;
    mat[1][0] = 4.4;
    mat[1][1] = 5.5;
    mat[1][2] = 666;
    prints("matrix height:");
    print(mat.height);
    prints("matrix width:");
    print(mat.width);
    return 0;
}
