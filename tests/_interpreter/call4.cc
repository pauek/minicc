#include <iostream>
using namespace std;

int f(int x) {
   return x + 1;
}

int main() {
   cout << f("hola") << endl;
}
[[out]]--------------------------------------------------
[[err]]--------------------------------------------------
tests/interpreter/call4.cc[1:0-1:0]: El argumento 1 no es compatible con el tipo del parámetro (int vs string)
