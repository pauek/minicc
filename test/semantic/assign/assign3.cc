#include <string>

void fac() {
   int a = 2;
   2 += a;
}
[[err]]--------------------------------------------------
semantic/assign/assign3.cc[5:3-5:9]: En el operador '+=' la parte izquierda debe ser una variable.
