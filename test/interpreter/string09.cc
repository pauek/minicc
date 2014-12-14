#include <iostream>
using namespace std;

int main() {
   string s(10, 'x');
   s[3] = '_';
   s[7] = '.';
   cout << s << endl;
}
[[out]]--------------------------------------------------
xxx_xxx.xx
