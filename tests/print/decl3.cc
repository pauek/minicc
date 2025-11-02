void f() {
    void a=b,b=c=d,c=d||x,d=2&u;
}
[[out]]--------------------------------------------------
void f() {
    void a = b, b = c = d, c = d || x, d = 2 & u;
}
