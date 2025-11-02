void f() {
        extern  volatile mutable register const   int x=1;
}
[[out]]--------------------------------------------------
void f() {
    const volatile mutable register extern int x = 1;
}
