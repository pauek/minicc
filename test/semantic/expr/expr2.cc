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
semantic/expr/expr2.cc[9:3-9:14]: Intentas sumar un 'int' y un 'string'.
semantic/expr/expr2.cc[10:3-10:14]: Intentas hacer un AND de un 'bool' con un 'string'.
semantic/expr/expr2.cc[11:3-11:12]: Intentas restar un 'float' y un 'bool'.
