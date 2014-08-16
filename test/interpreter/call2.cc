#include <iostream>
using namespace std;

int f(int x, int y) {
   return x + y;
}

int main() {
   int a = 10;
   cout << f(a, 2) << endl;
   int x = -2;
   cout << f(5, x) << endl;
}
[[out]]--------------------------------------------------
12
3
[[err]]--------------------------------------------------
