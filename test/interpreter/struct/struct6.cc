#include <iostream>
using namespace std;

struct A {
   int a, b;
};

struct B {
   int c;
   A d;
};

struct C {
   B e;
   float f;
};

int main() {
   C x = {{-10, {15, 709}}, .23};
   cout << x.f << ' ' << x.e.c << ' ' 
        << x.e.d.a << ' ' << x.e.d.b << endl;
}
[[out]]--------------------------------------------------
0.23 -10 15 709
