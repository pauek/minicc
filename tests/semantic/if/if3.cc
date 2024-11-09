#include <iostream>
using namespace std;

void f() {
   bool b = 5 > 6;
   int a;
   if (b) {
      a = 3;
   }
}

[[err]]----------------------------------------------------
semantic/if/if3.cc[7:8-7:9]: La condición siempre vale 'false'.
