#include <iostream>
#include <vector>
using namespace std;

int main() {
   vector<int> v(1, -13);
   v[0] += 5;
   cout << *v.begin() << endl;
}
[[out]]--------------------------------------------------
-8
