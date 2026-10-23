#include <iostream>
using namespace std;

int main() {
   string s = 1 + 3;
}
[[err]]----------------------------------------------------
tests/semantic/var/var_typemismatch2.cc[5:11-5:20]: El tipo del valor inicial ('int') no se corresponde con el tipo de la variable ('string').
