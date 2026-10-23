#include <iostream>
#include <vector>
using namespace std;

int main() {
   vector<int> v(3, -1);
   vector<int>::iterator it = v.end();
   it--;
   *it = 0;
   for (it = v.begin(); it != v.end(); it++) {
      cout << *it << ' ';
   }
   cout << endl;
}
[[out]]--------------------------------------------------
-1 -1 0 
