#include <iostream>
#include <vector>
using namespace std;

int main() {
   vector<int> v(5, -1);
   vector<int>::iterator it;
   for (it = v.begin(); it != v.end(); it++) {
      cout << *it << ' ';
   }
   cout << endl;
}
[[out]]--------------------------------------------------
-1 -1 -1 -1 -1 
