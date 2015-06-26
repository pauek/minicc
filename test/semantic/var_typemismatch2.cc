#include <iostream>
using namespace std;

int main() {
   string s = 1 + 3;
}
[[err]]----------------------------------------------------
semantic/var_typemismatch2.cc[5:10-5:19]: El tipo del valor inicial ('int') no se corresponde con el tipo de la variable ('string').
