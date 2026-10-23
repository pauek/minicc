#include <iostream>
#include <list>
using namespace std;

int main() {
   list<int> a(3, -2);
   list<int>::iterator it = a.begin();
   it++;
   *it += 3;
   it++;
   *it = 10;
   for (it = a.begin(); it != a.end(); it++) {
      cout << *it << ' ';
   }
   cout << endl;
}
[[out]]--------------------------------------------------
-2 1 10 
