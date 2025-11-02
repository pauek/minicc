void f() {
    a=b?/*asldkfjasdf*/x||y||z:z;
}
[[out]]--------------------------------------------------
void f() {
    a = b ? /*asldkfjasdf*/ x || y || z : z;
}
