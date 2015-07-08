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
semantic/func/func6.cc[12:11-12:15]: Número de argumentos erróneo (son 1 y deberían ser 0).
