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
semantic/assign/assign2.cc[5:3-5:15]: No se puede asignar un 'string' a una variable de tipo 'double'.
semantic/assign/assign2.cc[7:3-7:10]: No se puede asignar un 'char' a una variable de tipo 'bool'.
semantic/assign/assign2.cc[8:3-8:8]: No se puede asignar un 'int' a una variable de tipo 'bool'.
semantic/assign/assign2.cc[10:3-10:11]: No se puede asignar un 'bool' a una variable de tipo 'int'.
semantic/assign/assign2.cc[12:3-12:11]: No se puede asignar un 'double' a una variable de tipo 'char'.
semantic/assign/assign2.cc[14:3-14:11]: No se puede asignar un 'bool' a una variable de tipo 'string'.
semantic/assign/assign2.cc[15:3-15:14]: El tipo 'MadeUp' no existe.
