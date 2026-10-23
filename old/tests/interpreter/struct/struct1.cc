#include <iostream>
using namespace std;

struct Punto {
   double x, y;
};

int main() {
   Punto p = { 0.0, 0.5 };
   p.x += 0.1;
   p.y -= 0.1;
   cout << p.x << ' ' << p.y << endl;
}
[[out]]--------------------------------------------------
0.1 0.4
