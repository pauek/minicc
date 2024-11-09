#include <iostream>

void f() {
   if ("true") {
      std::cout << "yes!";
   }
}

[[err]]----------------------------------------------------
semantic/if/if1.cc[4:8-4:14]: La condición de un 'if' debe ser de tipo 'bool'.
