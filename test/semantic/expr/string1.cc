#include <string>

void f() {
   std::string a = "a", b = "b", c = "c";
   a = b - c;
}
[[err]]--------------------------------------------------
semantic/expr/string1.cc[5:7-5:12]: El tipo 'string' no tiene operador '-'.
