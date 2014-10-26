void f() {
   auto   extern  volatile mutable register const   int x=1;
}
[[out]]--------------------------------------------------
void f() {
   auto extern volatile mutable register const int x = 1;
}
