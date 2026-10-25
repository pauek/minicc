#include <iostream>
using namespace std;

int main() {
   int n = 0;
   string s;
   if (cin >> n >> s) {
      n++;        
   } else {
      s = "fail";
   }
   cout << n << " '" << s << "'" << endl;
}
[[in]]--------------------------------------------------
blah 5
[[out]]-------------------------------------------------
0 'fail'
