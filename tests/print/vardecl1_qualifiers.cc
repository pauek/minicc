void f() {
   auto   extern  volatile mutable register const   int x=1;
}
[[out]]--------------------------------------------------
void f() {
   const volatile mutable register auto extern int x = 1;
}
