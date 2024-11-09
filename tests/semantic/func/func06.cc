#include <iostream>
using namespace std;

void f() {
   int a = 3;
   cout << a << endl;
}

int main() {
   int a = 1;
   a = 3;
   int b = f(a);
   for (int i = 0; i < 10; i++) {
   	cout << "Hola, MiniC++" << endl;
   }
}
[[err]]--------------------------------------------------
tests/semantic/func/func06.cc[12:12-12:16]: Número de argumentos erróneo (son 1 y deberían ser 0).
