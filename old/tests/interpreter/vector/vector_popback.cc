#include <iostream>
#include <vector>
using namespace std;

int main() {
   vector<int> v(5, -1);
   v.pop_back();
   for (int i = 0; i < v.size(); i++) {
      cout << v[i] << ' ';
   }
   cout << endl;
}
[[out]]--------------------------------------------------
-1 -1 -1 -1 
