void f() {
   if ("true") {
      cout << "yes!";
   }
}

[[err]]----------------------------------------------------
semantic/if1.cc[2:7-2:13]: La condición de un 'if' debe ser de tipo 'bool'
