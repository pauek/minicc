#include <iostream>
using namespace std;

void f() {
   if (char(97)) {
      cout << "yes!";
   }
}

[[err]]----------------------------------------------------
semantic/if/if2.cc[5:7-5:15]: La condición de un 'if' debe ser de tipo 'bool'.
