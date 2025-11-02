int f() {
    if (a = b) { cout + x; } else { 10 + 1;
        if (a = b) { cout + x; } else { 1 + 10; } }
}
[[out]]--------------------------------------------------
int f() {
    if (a = b) {
        cout + x;
    } else {
        10 + 1;
        if (a = b) {
            cout + x;
        } else {
            1 + 10;
        }
    }
}
