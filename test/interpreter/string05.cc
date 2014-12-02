#include <iostream>
using namespace std;

int main() {
   string s = "abcdefghijklmno";
   cout << s.find("abc") << endl;
   cout << s.find("mno") << endl;
   cout << s.find("aaa") << endl;
   cout << s.find("abc", 3) << endl;
}
[[out]]--------------------------------------------------
0
12
-1
-1
