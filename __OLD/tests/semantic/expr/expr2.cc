#include <string>

void f() {
   int a = 1;
   bool b = true;
   char c = 'z';
   float f = 0.1;
   a += 2;
   a += "hola";
   b &= "hola";
   f -= true;
}
[[err]]--------------------------------------------------
tests/semantic/expr/expr2.cc[9:4-9:15]: Intentas sumar un 'int' y un 'string'.
tests/semantic/expr/expr2.cc[10:4-10:15]: Intentas hacer un AND de un 'bool' con un 'string'.
tests/semantic/expr/expr2.cc[11:4-11:13]: Intentas restar un 'float' y un 'bool'.
