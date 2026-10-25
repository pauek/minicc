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
tests/semantic/struct/struct7.cc[12:4-12:7]: El campo 'f' no existe.
