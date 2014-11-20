#include <iostream>
#include <vector>
using namespace std;

struct A { int x, y; };

int main() {
   A a = {1, 2};
   vector<A> v(5, a);
   v[2].x = -5;
   v[3].y = -3;
   for (int i = 0; i < 5; i++) {
      if (i > 0) { cout << ", "; }
      cout << v[i].x << ' ' << v[i].y;
   }
   cout << endl;
}
[[out]]--------------------------------------------------
1 2, 1 2, -5 2, 1 -3, 1 2
