#include <list>
#include <iostream>
using namespace std;

int main() {
   list<int> L;
   L.push_back(5);
   L.push_front(1);
   list<int>::iterator it;
   for (it = L.begin(); it != L.end(); it++) {
      cout << *it << ' ';
   }
   cout << endl;
}
[[out]]--------------------------------------------------
1 5 
