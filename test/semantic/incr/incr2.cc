#include <string>

void f() {
   std::string s = "1";
   s++;
}
[[err]]--------------------------------------------------
semantic/incr/incr2.cc[5:3-5:6]: El tipo 'string' no tiene operador '++'.
