#include <iostream>
using namespace std;

struct Punto {
   double x, y;
};

int main() {
   Punto p = { 0.0, 0.5, 1.0 };
   p.x += 0.1;
   p.y -= 0.1;
   cout << p.x << ' ' << p.y << endl;
}
[[out]]--------------------------------------------------
[[err]]--------------------------------------------------
tests/interpreter/struct/struct2.cc[1:0-1:0]: Demasiados valores al inicializar la tupla de tipo 'Punto'
