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
semantic/if3.cc[7:7-7:8]: La condición siempre vale 'false'.
