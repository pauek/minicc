#include <string>

void fac() {
   int a = 2;
   a += "hola";
   std::string s = "hi";
   s += 3;
   int x = 0, b = 1;
   x |= s;
   b &= s;
}
[[err]]--------------------------------------------------
semantic/assign/assign4.cc[5:4-5:15]: Intentas sumar un 'int' y un 'string'.
semantic/assign/assign4.cc[7:4-7:10]: Intentas sumar un 'string' y un 'int'.
semantic/assign/assign4.cc[9:4-9:10]: Intentas hacer un OR de un 'int' con un 'string'.
semantic/assign/assign4.cc[10:4-10:10]: Intentas hacer un AND de un 'int' con un 'string'.
