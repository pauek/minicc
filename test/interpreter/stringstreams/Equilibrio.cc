#include <iostream>
#include <sstream>
using namespace std;

int S(string s) {
   istringstream S(s);
   int x, t = 0;
   while (S >> x) t += x;
   return t;
}

int main() {
   string L;
   while (getline(cin, L)) {
      int pos = L.find('-');
      int s1 = S(L.substr(0, pos));
      int s2 = S(L.substr(pos+1));
      if (s1 == s2) {
         cout << "si" << endl;
      } else {
         cout << "no" << endl;
      }
   }
}
[[in]]--------------------------------------------------
1 3-2 2
5-3 2
10-1
1 1 1 1 1 1 1 1 1 1-2 2 2 2 2
0 0 0 0 0-0 1 2 3 4
[[out]]-------------------------------------------------
si
si
no
si
no
