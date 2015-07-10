void f() {
   int a = 1;
   bool b = !a;
}
[[err]]--------------------------------------------------
semantic/neg/neg1.cc[3:12-3:14]: Sólo se puede negar una expresión de tipo 'bool'.
