#include <iostream>
using namespace std;

int main() {
   string s, t;
   while (cin >> s) {
      if (s[0] == '-') {
         t += s[s.size()-1];
         cout << t << endl;
      }
   }
}
[[in]]--------------------------------------------------
0 -1 8 -22 -79 15 +7 -.
[[out]]-------------------------------------------------
1
12
129
129.
