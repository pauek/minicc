
#include <iostream>
using namespace std;

int main() {
   int x, p = -1, s = -1, t = -1;
   while (cin >> x) {
      if (p == -1 || x > p) {
         t = s; s = p; p = x;
      } else if (s == -1 || x > s) {
         t = s; s = x;
      } else if (t == -1 || x > t) {
         t = x;
      }
   }
   cout << p << ' ' << s << ' ' << t << endl;
}
[[in]]--------------------------------------------------
13 0 2 1 9 7 4 20 6 -1 -5 10 14 12 9 8 -10 -20
[[out]]-------------------------------------------------
20 14 13
