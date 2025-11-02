int f ( ) {
    const map</*A*/int/*B*/,/*C*/string/*D*/> M;
}
[[out]]--------------------------------------------------
int f() {
    const map</*A*/ int /*B*/, /*C*/ string /*D*/> M;
}
