#include <iostream>
#include <vector>
using namespace std;

struct A { int x, y; };

int main() {
   vector<A> v(1);
   v[0].x = -3;
   v[0].y = -5;
   vector<A>::iterator it = v.begin();
   cout << it->x << ' ' << it->y << endl;
}
[[out]]--------------------------------------------------
-3 -5
