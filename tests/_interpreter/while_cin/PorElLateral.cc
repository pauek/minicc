#include <iostream>
using namespace std;

int main() {
   char p;
   string f, S, a, b;
   cin >> a >> b;
   getline(cin, f);
   while (cin >> p) {
      getline(cin, f);
      if (p == a[0]) {
         S = S + f;
      } else if (p == b[0]) {
         S = f + S;
      }
   }
   cout << S << endl;
}
[[in]]--------------------------------------------------
+ -
# As they say:
- you're not
- is free,
+ the customer,
-If something online
+ you're the product.
[[out]]-------------------------------------------------
If something online is free, you're not the customer, you're the product.
