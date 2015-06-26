#include <iostream>
using namespace std;

int f(int a) {
   return a + 3;
}

int main() {
   string s = f(7);
}
[[err]]----------------------------------------------------
semantic/var_typemismatch3.cc[9:10-9:18]: El tipo del valor inicial ('int') no se corresponde con el tipo de la variable ('string').
