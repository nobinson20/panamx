int main() {
    matrix m;
    matrix n;
    matrix q;
    m = [1, 9, 3;
         2, 5, 7;
         3, 6, 4];
    n = [17.1, 23.2, 10.9;
         15.6, 21.7, 10.5;
         9.8,  18.2, 21.5];
    q = m + n;
    printm(q);
    q = n - m;
    printm(q);
    return 0;
}
