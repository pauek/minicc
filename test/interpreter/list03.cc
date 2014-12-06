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
   L.reverse();
   list<string>::iterator it;
   for (it = L.begin(); it != L.end(); it++) {
      cout << *it << ' ';
   }
   cout << endl;          
}
[[in]]---------------------------------------------------
a can de morl .
[[out]]--------------------------------------------------
morl de can a 
