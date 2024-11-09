#include <iostream>
#include <list>
using namespace std;

struct X {
   int a, b;
};

int main() {
   X x = {7, 13};
   list<X> L;
   L.resize(3, x);
   list<X>::iterator it;
   for (it = L.begin(); it != L.end(); it++) {
      cout << it->a << ' ' << it->b << endl;
   }
}
