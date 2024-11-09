#include <iostream>
#include <vector>
using namespace std;

int main() {
   vector<int> v(5);
   for (int i = 0; i < 5; i++) {
      v[i] = i+1;
   }
   vector<int>::iterator it = v.begin();
   it = it + 3;
   *it = -4;
   *(v.begin() + 1) = -2;
   for (it = v.begin(); it != v.end(); it++) {
      cout << *it << ' ';
   }
   cout << endl;
}
[[out]]--------------------------------------------------
1 -2 3 -4 5 
