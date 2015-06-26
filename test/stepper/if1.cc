#include <iostream>
using namespace std;

int main() {
   int a;
   cin >> a;
   if (a == 42) {
      cout << "yes" << endl;
   } else {
      cout << "no" << endl;
   }
}
[[in]]---------------------------------------------------
41
[[out]]--------------------------------------------------
4:4-4:8: main
Empieza el programa.

5:3-5:8: int a
Se declara la variable 'a'.

6:3-6:11: cin >> a
Se lee de la entrada.

7:7-7:14: a == 42
La condición vale 'false', seguimos por la segunda rama.

10:14-10:18: "no"
Se escribe a la salida.
OUTPUT: "no"

10:22-10:26: endl
Se escribe a la salida.
OUTPUT: "
"

12:0-12:1: }
Termina el programa.

