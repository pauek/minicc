#include <iostream>
using namespace std;

string R(string s) {
   string res;
   for (int i = s.size()-1; i >= 0; i--) {
      res += s[i];
   }
   return res;
}

int main() {
   string s;
   cin >> s;
   cout << R(s) << endl;
}
[[in]]---------------------------------------------------
hhoollaaaaa
[[out]]--------------------------------------------------
aaaaalloohh
