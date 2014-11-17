#include <iostream>
using namespace std;

int main() {
   vector<int> a(3, -2);
   a[2] = 10;
   a[1] += 3;
   cout << a[0] << ' ' << a[1] << ' ' << a[2] << endl;
}
[[out]]--------------------------------------------------
-2 1 10
