#include <iostream>
#include <list>
using namespace std;

int main() {
   list<int> L;
   for (int i = 0; i < 5; i++) {
      L.push_back(i*5);
   }
   list<int>::iterator it = L.begin();
   it++; it++;
   it = L.erase(it);
   L.erase(L.begin());
   for (it = L.begin(); it != L.end(); it++) {
      cout << *it << ' ';
   }
   cout << endl;
}
[[out]]--------------------------------------------------
5 15 20 
