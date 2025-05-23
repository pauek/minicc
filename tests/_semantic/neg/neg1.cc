void f() {
   int a = 1;
   bool b = !a;
}
[[err]]--------------------------------------------------
tests/semantic/neg/neg1.cc[3:13-3:15]: Sólo se puede negar una expresión de tipo 'bool'.
