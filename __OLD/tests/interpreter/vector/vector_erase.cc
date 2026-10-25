#include <iostream>
#include <vector>
using namespace std;

int main() {
   vector<int> v;
   for (int i = 0; i < 5; i++) {
      v.push_back(i*5);
   }
   vector<int>::iterator it = v.begin();
   it++; it++;
   it = v.erase(it);
   v.erase(v.begin());
   for (int i = 0; i < v.size(); i++) {
      cout << v[i] << ' ';
   }
   cout << endl;
}
[[out]]--------------------------------------------------
5 15 20 
