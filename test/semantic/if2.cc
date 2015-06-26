void f() {
   if (char(97)) {
      cout << "yes!";
   }
}

[[err]]----------------------------------------------------
semantic/if2.cc[2:7-2:15]: La condición de un 'if' debe ser de tipo 'bool'
