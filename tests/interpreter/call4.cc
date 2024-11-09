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
Error de ejecución: El argumento 1 no es compatible con el tipo del parámetro (int vs string)
