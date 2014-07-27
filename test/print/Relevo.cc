#include <iostream>
#include <sstream>
using namespace std;

int main() {
   string L, p1, p2, w;
   getline(cin, L);
   istringstream A(L);
   A >> p1 >> p2;
   while (getline(cin, L)) {
      istringstream B(L);
      int i = 0;
      while (B >> w) {
         if (i > 0) cout << " ";
         if (w == p1) {
            cout << p2;
         } else {
            cout << w;
         }
         i++;
      }
      cout << endl;
   }
}
[[out]]--------------------------------------------------
#include <iostream>
#include <sstream>
using namespace std;

int main() {
   string L, p1, p2, w;
   getline(cin, L);
   istringstream A(L);
   A >> p1 >> p2;
   while (getline(cin, L)) {
      istringstream B(L);
      int i = 0;
      while (B >> w) {
         if (i > 0) cout << " ";
         if (w == p1) {
            cout << p2;
         } else {
            cout << w;
         }
         i++;
      }
      cout << endl;
   }
}
[[err]]--------------------------------------------------
