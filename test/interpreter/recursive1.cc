#include <iostream>
using namespace std;

int f(int x) {
   if (x > 0) {
      return 2 * f(x-1);
   } else {
      return 1;
   }
}

int main() {
   cout << f(5) << endl;
}
[[out]]--------------------------------------------------
32
[[err]]--------------------------------------------------
