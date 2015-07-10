void f() {
   int a = 2;
   3 = a;
}
[[err]]--------------------------------------------------
semantic/assign/assign1.cc[3:3-3:8]: Intentas asignar sobre algo que no es una variable.
