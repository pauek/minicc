#include <iostream>
using namespace std;


int main() {
   int sz = 5;
   int a[sz];
   a[4] = 17;
   a[0] = -13;
   cout << a[0] << ' ' << a[4] << endl;
}
[[out]]--------------------------------------------------
-13 17
