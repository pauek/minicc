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
tests/semantic/expr/expr1.cc[8:8-8:13]: No se puede sumar un 'int' con un 'bool'.
tests/semantic/expr/expr1.cc[9:8-9:13]: No se puede sumar un 'char' con un 'bool'.
tests/semantic/expr/expr1.cc[11:8-11:13]: No se puede sumar un 'bool' con un 'double'.
