#include <map>
#include <iostream>
using namespace std;

int main() {
   map<int, string> M;
   string s;
   int n = 0;
   while (cin >> s) {
      M[n] = s;
      cout << M[n] << endl;
      n++;
   }
}
[[in]]---------------------------------------------------
a bb ccc dddd eeeee ffffff
[[out]]--------------------------------------------------
a
bb
ccc
dddd
eeeee
ffffff
