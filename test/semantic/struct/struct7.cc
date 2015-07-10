#include <iostream>
using namespace std;

struct X {
   char c;
   double d;
   string s;
};

int main() {
   X x;
   x.f = 1;
}
[[err]]--------------------------------------------------
semantic/struct/struct7.cc[12:3-12:6]: El campo 'f' no existe.
