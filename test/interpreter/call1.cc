#include <iostream>
using namespace std;

int f(int x) {
   return x + 22;
}

int main() {
   int a = 10;
   cout << f(a) << endl;
   int x = -2;
   cout << f(x) << endl;
}
[[out]]--------------------------------------------------
32
20
