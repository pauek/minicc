#include <iostream>
#include <vector>
using namespace std;

int main() {
   vector<int> v(1, -13);
   v[0] += 1;
   vector<int>::iterator i;
   i = v.begin();
   cout << *i << endl;
}
[[out]]--------------------------------------------------
-12
