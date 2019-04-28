struct Node {
    int x;
    int y;
};

int main() {
    struct Node p;
    p = <struct Node>;
    p.x = 121;
    p.y = 999;
    print(p.x);
    print(p.y);
    return 0;
}
