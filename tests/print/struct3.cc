struct X {
    int a, b;
};

int main() {
    X x={1, 3};
}
[[out]]--------------------------------------------------
struct X {
    int a, b;
};

int main() {
    X x = {1, 3};
}
