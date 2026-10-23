#include <iostream>
using namespace std;

int f(int a) {
   return a + 3;
}

int main() {
   string s = f(7);
}
[[err]]----------------------------------------------------
tests/semantic/var/var_typemismatch3.cc[9:11-9:19]: El tipo del valor inicial ('int') no se corresponde con el tipo de la variable ('string').
