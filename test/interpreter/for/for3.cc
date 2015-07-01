#include <iostream>
using namespace std;

int main() {
   bool a = true;
   for (; a;) {
      cout << "a" << endl;
      a = false;
   }
   cout << a << endl;
}
[[out]]--------------------------------------------------
a
0
