#include <iostream>
using namespace std;

struct A {
   int na, a[10];
};

int main() {
   A x = { 3, {-1, -2, -3, -4, 0, 1} };
   x.na = 0;
   for (int i = 5; i < 10; i++) {
   	x.a[i] = (i-10)*2;
   }
   cout << x.na << endl;
   for (int i = 0; i < 10; i++) {
      cout << x.a[i] << ' ';
   }
   cout << endl;
}
[[out]]--------------------------------------------------
0
-1 -2 -3 -4 0 -10 -8 -6 -4 -2 
[[err]]--------------------------------------------------
