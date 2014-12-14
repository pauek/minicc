#include <iostream>
#include <list>
using namespace std;

int main() {
   list<string> L;
   string p;
   cin >> p;
   while (p != ".") {
      L.push_back(p);
      cin >> p;
   }
   L.unique();
   list<string>::iterator it;
   for (it = L.begin(); it != L.end(); it++) {
      cout << *it << ' ';
   }
   cout << endl;          
}
[[in]]---------------------------------------------------
a a a b b c d d e a b b b b c .
[[out]]--------------------------------------------------
a b c d e a b c 
