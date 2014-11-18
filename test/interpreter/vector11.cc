#include <iostream>
using namespace std;

int main() {
   vector<int> v;
   cout << v.size() << endl;
   v.push_back(13);
   cout << v.size() << ' ' << v[0] << endl;
}
[[out]]--------------------------------------------------
0
1 13
