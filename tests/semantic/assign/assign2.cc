#include <string>

void fac() {
   double d;
   d = "no way";
   bool x;
   x = 'z';
   x = 2;
   int a;
   a = true;
   char z;
   z = 0.01;
   std::string s;
   s = true;
   MadeUp blah;
   blah = 1;
   float f;
   f = 1;
   f = 0.02f;
}
[[err]]--------------------------------------------------
tests/semantic/assign/assign2.cc[5:4-5:16]: No se puede asignar un 'string' a una variable de tipo 'double'.
tests/semantic/assign/assign2.cc[7:4-7:11]: No se puede asignar un 'char' a una variable de tipo 'bool'.
tests/semantic/assign/assign2.cc[8:4-8:9]: No se puede asignar un 'int' a una variable de tipo 'bool'.
tests/semantic/assign/assign2.cc[10:4-10:12]: No se puede asignar un 'bool' a una variable de tipo 'int'.
tests/semantic/assign/assign2.cc[12:4-12:12]: No se puede asignar un 'double' a una variable de tipo 'char'.
tests/semantic/assign/assign2.cc[14:4-14:12]: No se puede asignar un 'bool' a una variable de tipo 'string'.
tests/semantic/assign/assign2.cc[15:4-15:15]: El tipo 'MadeUp' no existe.
