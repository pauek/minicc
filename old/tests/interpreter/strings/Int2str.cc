#include <iostream>
using namespace std;

string int2str(int x) {
   int xc = x, nc = 0;
   while (xc > 0) xc /= 10, nc++;
   string s(nc, 'x');
   for (int i = 0; i < nc; i++) {
      s[nc - 1 - i] = ('0' + x % 10);
      x /= 10;
   }
   return s;
}

int main() {
   cout << int2str(1001) << endl;
}
[[out]]--------------------------------------------------
1001
