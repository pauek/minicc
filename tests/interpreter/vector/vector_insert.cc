#include <iostream>
#include <vector>
using namespace std;

int main() {
   vector<int> v(3, 17);
   vector<int>::iterator it = v.begin();
   it++; it++;
   it = v.insert(it, 2);
   it = v.insert(it, 0);
   for (int i = 0; i < v.size(); i++) {
      cout << v[i] << ' ';
   }
   cout << endl;
}
[[out]]--------------------------------------------------
17 17 0 2 17 
