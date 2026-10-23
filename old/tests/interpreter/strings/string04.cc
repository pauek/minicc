#include <iostream>
using namespace std;

int main() {
   string s = "xxx___xxx";
   string t = "yyyyy****";
   s.erase(3, 3);
   t.erase(5);
   cout << s << endl;
   cout << t << endl;
}
[[out]]--------------------------------------------------
xxxxxx
yyyyy
