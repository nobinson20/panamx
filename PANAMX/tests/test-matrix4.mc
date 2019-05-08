int main() {
    matrix mat;
    mat = <3, 3>;
    prints("original matrix:");

    matrix oth;
    oth = <4, 3>;

    mat[0][0] = 0;
    mat[0][1] = 1;
    mat[0][2] = 3;
    mat[1][0] = 0;
    mat[1][1] = 2;
    mat[1][2] = 3;
    mat[2][0] = 2;
    mat[2][1] = 4;
    mat[2][2] = 1;

    printm(mat);

    return 0;
}

