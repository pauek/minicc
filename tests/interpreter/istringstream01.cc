#include <sstream>
#include <iostream>
using namespace std;

int main() {
   istringstream S("bla bli blu");
   string p;
   while (S >> p) {
      cout << p << endl;
   }
}
[[out]]--------------------------------------------------
bla
bli
blu
