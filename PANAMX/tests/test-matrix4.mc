int main() {
    matrix m;
    matrix n;
    m = [1, 2; 3, 4];
    n = 8.5 + m;
    printm(n);
    m = n - 1.5;
    printm(m);
    n = m * 2;
    printm(n);
    m = n / 4;
    printm(m);
    return 0;
}
