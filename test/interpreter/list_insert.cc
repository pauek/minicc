#include <iostream>
#include <list>
using namespace std;

int main() {
   list<int> L(3, 17);
   list<int>::iterator it = L.begin();
   it++; it++;
   it = L.insert(it, 2);
   it = L.insert(it, 0);
   for (it = L.begin(); it != L.end(); it++) {
      cout << *it << ' ';
   }
   cout << endl;
}
[[out]]--------------------------------------------------
17 17 0 2 17 
