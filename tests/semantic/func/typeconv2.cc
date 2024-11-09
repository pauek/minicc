#include <string>
using namespace std;

void f() {
   string s;
   s = string(1);
}
[[err]]--------------------------------------------------
semantic/func/typeconv2.cc[6:8-6:17]: No se puede convertir un 'int' en un 'string'.
