#include <iostream>
using namespace std;

int main() {
   int a = 2;
   a = a + 3;
}
[[out]]--------------------------------------------------
4:5-4:9: main
Empieza el programa.

5:4-5:13: int a = 2
Se declara la variable 'a'.

6:8-6:13: a + 3
La expresión ha dado 5.

6:4-6:8: a = 
Asignamos el valor.

7:1-7:2: }
Termina el programa.

