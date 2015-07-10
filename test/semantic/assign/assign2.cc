#include <string>

void fac() {
   double d;
   d = "no way";
   bool x;
   x = 'z';
   int a;
   a = true;
   char z;
   z = 0.01;
   std::string s;
   s = true;
   MadeUp blah;
   blah = 1;
}
[[err]]--------------------------------------------------
semantic/assign/assign2.cc[5:3-5:15]: No se puede asignar un 'string' a una variable de tipo 'double'.
semantic/assign/assign2.cc[7:3-7:10]: No se puede asignar un 'char' a una variable de tipo 'bool'.
semantic/assign/assign2.cc[11:3-11:11]: No se puede asignar un 'double' a una variable de tipo 'char'.
semantic/assign/assign2.cc[13:3-13:11]: No se puede asignar un 'bool' a una variable de tipo 'string'.
semantic/assign/assign2.cc[14:3-14:14]: El tipo 'MadeUp' no existe.
