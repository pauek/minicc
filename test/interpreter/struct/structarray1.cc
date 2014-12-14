#include <iostream>
using namespace std;

struct A {
   int a, b[3], c;
};

int main() {
   A x = {-10, {-1, 7, 23}, -1023};
   cout << x.a;
   for (int i = 0; i < 3; i++) {
      cout << ' ' << x.b[i];
   }
   cout << ' ' << x.c << endl;
}
[[out]]--------------------------------------------------
-10 -1 7 23 -1023
