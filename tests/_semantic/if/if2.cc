#include <iostream>
using namespace std;

void f() {
   if (char(97)) {
      cout << "yes!";
   }
}

[[err]]----------------------------------------------------
tests/semantic/if/if2.cc[5:8-5:16]: La condición de un 'if' debe ser de tipo 'bool'.
