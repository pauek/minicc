#macro hello(ast s) {
   return #{
      #include <stdlib.h>
      int main() {
         printf(#(s));
      }
   }
}

#hello("world")

