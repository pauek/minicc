void f() {
   int a = 2;
   3 = a;
}
[[err]]--------------------------------------------------
semantic/assign/assign1.cc[3:4-3:9]: Intentas asignar sobre algo que no es una variable.
