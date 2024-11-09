#include <string>

void fac() {
   int a = 2;
   2 += a;
}
[[err]]--------------------------------------------------
semantic/assign/assign3.cc[5:4-5:10]: En el operador '+=' la parte izquierda debe ser una variable.
