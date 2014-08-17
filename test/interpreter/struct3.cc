#include <iostream>
using namespace std;

struct Punto {
   double x, y;
};

int main() {
   Punto p = { 0.0 };
   p.x += 0.1;
   p.y = 0.3;
   cout << p.x << ' ' << p.y << endl;
}
[[out]]--------------------------------------------------
0.1 0.3
[[err]]--------------------------------------------------
