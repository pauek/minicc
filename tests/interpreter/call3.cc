#include <iostream>
using namespace std;

int f(int x, int y) {
   x + y;
}

int main() {
   int a = 10;
   cout << f(a, 2) << endl;
   int x = -2;
   cout << f(5, x) << endl;
}
[[out]]--------------------------------------------------
[[err]]--------------------------------------------------
Error de ejecución: La función 'f' debería devolver un 'int'
