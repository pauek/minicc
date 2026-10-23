#include <iostream>
#include <list>
#include <sstream>
using namespace std;

int main() {
   istringstream S("1 5 3 2 7 -2 9");
   list<int> L;
   int n;
   while (S >> n) { 
      L.push_back(n); 
   }
   L.sort();
   list<int>::iterator it;
   for (it = L.begin(); it != L.end(); it++) {
      cout << *it << ' ';
   }
   cout << endl;
}
[[out]]--------------------------------------------------
-2 1 2 3 5 7 9 
