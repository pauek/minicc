void f() {
   g(/* ji ji */a + b);
}
[[out]]--------------------------------------------------
void f() {
   g(/* ji ji */ a + b);
}
[[err]]--------------------------------------------------