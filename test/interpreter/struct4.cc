#include <iostream>
using namespace std;

struct A {
   int a, b;
};

struct B {
   int c;
   A d;
};

int main() {
   B x;
   x.c = 1;
   x.d.a = 2;
   x.d.b = 3;
   cout << x.c << ' ' << x.d.a << ' ' << x.d.b << endl;
}
[[out]]--------------------------------------------------
1 2 3
[[err]]--------------------------------------------------
