#include <iostream>
#include <list>
using namespace std;

int main() {
   list<int> a(3, -2);
   a.resize(6, 2);
   a.pop_back();
   a.push_back(1);
   a.pop_front();
   a.push_front(-1);
   list<int>::iterator it;
   for (it = a.begin(); it != a.end(); it++) {
      cout << *it << ' ';
   }
   cout << endl;
}
[[out]]--------------------------------------------------
-1 -2 -2 2 2 1 
