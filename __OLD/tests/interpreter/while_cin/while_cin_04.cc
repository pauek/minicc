#include <iostream>
using namespace std;

int main() {
   string p, s;
   while (cin >> p) {
      s += "_";
      s += p;
   }
   cout << s << endl;
}
[[in]]---------------------------------------------------------
hi there    pal.    How     are     you!           
[[out]]--------------------------------------------------------
_hi_there_pal._How_are_you!
