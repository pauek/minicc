#include <iostream>
using namespace std;

int main() {
   int a = 0, b = 1;
   while (b < 1000) {
      cout << b << ' ';
      int _a = a;
      a = b;
      b = _a + b;
   }
   cout << endl;
}
[[out]]--------------------------------------------------
1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 
