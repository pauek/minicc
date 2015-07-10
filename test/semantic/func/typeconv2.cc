#include <string>
using namespace std;

void f() {
   string s;
   s = string(1);
}
[[err]]--------------------------------------------------
semantic/func/typeconv2.cc[6:7-6:16]: No se puede convertir un 'int' en un 'string'.
