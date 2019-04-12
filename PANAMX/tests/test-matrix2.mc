int main() {
    matrix<int>[3][3] m;
    int i;
    int j;
    int k;

    m = [-1, -2, -3; -4, -5, -6; 6, 7, 8;];
    k = 100;
    for (i = 0; i < 3; i++) {
        for (j = 0; j < 3; j++) {
            m[i][j] = k;
            k++;
        }
    }
    for (i = 0; i < 3; i++) {
        for (j = 0; j < 3; j++) {
            print(m[i][j]);
        }
    }
    return 0;
}
