#include <iostream>
using namespace std;

int main() {
   string p, up = " ";
   while (cin >> p) {
      int a = p.size(), b = up.size();
      if (a == b || (p[0] == up[0] && p[a-1] == up[b-1])) {
         cout << p << ' ';
      }
      up = p;
   }
   cout << endl;
}
[[in]]---------------------------------------------------
__ It is very nice uh to be input important and but it is mme more 
important important two to be real nice
[[out]]--------------------------------------------------
It is nice to be important but is more important to be nice 
