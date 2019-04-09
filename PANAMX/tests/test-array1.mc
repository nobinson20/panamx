int main() {
    int i;
    int[5] a;
    double[3] b;
    a = [1024,22,33,-666,128];
    b = [-1.23, 5.56, 7.62];
    for (i = 0; i < 5; i++) {
        print(a[i]);
    }
    for (i = 0; i < 3; i++) {
        printf(b[i]);
    }
    return 0;
}
