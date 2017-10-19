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
4:5-4:9: main
Empieza el programa.

5:4-5:9: int a
Se declara la variable 'a'.

6:4-6:12: cin >> a
Se lee de la entrada.

7:8-7:15: a == 42
La condición vale 'false', seguimos por la segunda rama.

10:15-10:19: "no"
Se escribe a la salida.
OUTPUT: "no"

10:23-10:27: endl
Se escribe a la salida.
OUTPUT: "
"

12:1-12:2: }
Termina el programa.

