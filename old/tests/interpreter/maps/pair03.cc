#include <iostream>
#include <list>
#include <map>
using namespace std;

int main() {
   list<pair<int,int>> L;
   pair<int,int> p = {1, 3}, q = {3, 4}, r = {1, 2}, t = {2, 5};
   L.push_back(p);
   L.push_back(q);
   L.push_back(r);
   L.push_back(t);
   L.sort();
   list<pair<int,int>>::iterator it = L.begin();
   for (it = L.begin(); it != L.end(); it++) {
      cout << (*it).first << ", " << (*it).second << endl;
   }
}
[[out]]--------------------------------------------------
1, 2
1, 3
2, 5
3, 4
