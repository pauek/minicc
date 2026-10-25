#include <iostream>
using namespace std;

int main() {
   string a, b, s;
   cin >> a >> b;
   s = a + b;
   int sz = s.size();
   for (int i = 0; i < b.size(); i++) {
      s[sz - 1 - i] = b[i];
   }
   cout << s << endl;
}
[[in]]--------------------------------------------------
blah qwerty
[[out]]-------------------------------------------------
blahytrewq
