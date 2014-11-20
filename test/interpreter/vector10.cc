#include <iostream>
#include <vector>
using namespace std;

int main() {
   vector<char> v(3, 'b');
   v.back() = 'z';
   v.front() = 'a';
   cout << v[0] << ' ' << v[1] << ' ' << v[2] << endl;
   cout << v.front() << ' ' << v.back() << endl;
}
[[out]]--------------------------------------------------
a b z
a z
