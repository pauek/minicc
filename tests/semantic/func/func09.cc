#include <iostream>
using namespace std;

void f(int x) {
   cout << x+1 << endl;
}

int main() {
   int a = 2, b;
   b = f(a);
}
[[err]]--------------------------------------------------
tests/semantic/func/func09.cc[10:4-10:12]: No se puede asignar un 'void' a una variable de tipo 'int'.
