#include <iostream>
using namespace std;

struct X {
   char c;
   bool b;
};

void g(X& x) {
   int y, z;
   x.c = 'z';
   for (int j = 0; j < 5; j++) {
      cout << j << endl;
   }
}

void f(int& x) {
   X a = { '3', false };
   int b = 7, c;
   cout << a.c << endl;
   g(a);
   cout << a.b << endl;
}

int main() {
   int a = 1, b = 6;
   a = 3;
   f(b);
   for (int i = 0; i < 10; i++) {
      cout << "Hola, MiniC++" << endl;
   }
}
[[err]]--------------------------------------------------
