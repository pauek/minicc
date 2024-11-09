#include <string>

void f() {
   std::string s = "1";
   s++;
}
[[err]]--------------------------------------------------
tests/semantic/incr/incr2.cc[5:4-5:7]: El tipo 'string' no tiene operador '++'.
