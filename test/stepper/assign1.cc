#include <iostream>
using namespace std;

int main() {
   int a = 2;
   a = a + 3;
}
[[out]]--------------------------------------------------
4:4-4:8: main
Empieza el programa.

5:3-5:13: int a = 2;
Se declara la variable 'a'.

6:7-6:12: a + 3
La expresión ha dado 5.

6:3-6:7: a = 
Asignamos el valor.

7:0-7:1: }
Termina el programa.

[[err]]--------------------------------------------------
