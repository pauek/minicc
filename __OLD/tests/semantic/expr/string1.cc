#include <string>

void f() {
   std::string a = "a", b = "b", c = "c";
   a = b - c;
}
[[err]]--------------------------------------------------
tests/semantic/expr/string1.cc[5:8-5:13]: El tipo 'string' no tiene operador '-'.
