#include <iostream>
using namespace std;

int main() {
   char c;
   while (cin >> c) {
      cout << int(c) - int('a');
   }
   cout << endl;
}
[[in]]---------------------------------------------------
a   f
 b g
  c
 h d
i   e
[[out]]--------------------------------------------------
051627384
