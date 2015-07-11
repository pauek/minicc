#include <string>

void f() {
   int a = 1;
   bool b = true;
   char c = 'z';
   std::string s;
   s = a + b;
   s = c + b;
   double d = 0.001;
   s = b + d;
}
[[err]]--------------------------------------------------
semantic/expr/expr1.cc[8:7-8:12]: No se puede sumar un 'int' con un 'bool'.
semantic/expr/expr1.cc[9:7-9:12]: No se puede sumar un 'char' con un 'bool'.
semantic/expr/expr1.cc[11:7-11:12]: No se puede sumar un 'bool' con un 'double'.
