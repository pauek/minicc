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
semantic/assign/assign4.cc[5:3-5:14]: Intentas sumar un 'int' y un 'string'.
semantic/assign/assign4.cc[7:3-7:9]: Intentas sumar un 'string' y un 'int'.
semantic/assign/assign4.cc[9:3-9:9]: Intentas hacer un OR de un 'int' con un 'string'.
semantic/assign/assign4.cc[10:3-10:9]: Intentas hacer un AND de un 'int' con un 'string'.
