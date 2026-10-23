#include <iostream>
using namespace std;

int main() {
   string p;
   while (cin >> p) {
      int k = 0, i = p.find(p[0], 1);
      while (i != string::npos) {
         k++;
         i = p.find(p[0], i+1);
      }
      cout << k << ' ';
   }
   cout << endl;
}
[[in]]---------------------------------------------------
xax
abcdef
100122181
[[out]]--------------------------------------------------
1 0 3 
