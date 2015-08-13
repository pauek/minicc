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
semantic/func/func9.cc[10:3-10:11]: No se puede asignar un 'void' a una variable de tipo 'int'.
