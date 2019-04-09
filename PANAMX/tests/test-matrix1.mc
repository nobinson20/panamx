int main() {
    int i;
    int j;
    matrix<int>[3][2] m;
    matrix<double>[2][3] a;

    m = [11, 22; 33, 44; 55, 66;];
    for (i = 0; i < 3; i++) {
        for (j = 0; j < 2; j++) {
            print(m[i][j]);
        }
    }

    a = [1.1, 2.2, 3.3; 4.4, 5.5, 6.6;];
    for (i = 0; i < 2; i++) {
        for (j = 0; j < 3; j++) {
            printf(a[i][j]);
        }
    }
    return 0;
}
