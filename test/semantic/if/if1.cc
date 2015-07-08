#include <iostream>

void f() {
   if ("true") {
      std::cout << "yes!";
   }
}

[[err]]----------------------------------------------------
semantic/if/if1.cc[4:7-4:13]: La condición de un 'if' debe ser de tipo 'bool'.
