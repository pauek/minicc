#include <iostream>
using namespace std;

int main() {
   int a[6] = {1, 1, 1, 1, 1, 1};
   int b = 2, c = 3;
   a[0] &= b; a[1] &= c;
   a[2] |= b; a[3] |= c;
   a[4] ^= b; a[5] ^= c;
   for (int i = 0; i < 3; i++) {
      cout << a[2*i] << ' ' << a[2*i+1] << endl;
   }
}
[[out]]--------------------------------------------------
0 1
3 3
3 2
