#include <iostream>
using namespace std;

int f(int a, int b) {
   return a * b;
}

int main() {
   int x = 3;
   cout << f(x, 7) << endl;
}
[[out]]--------------------------------------------------
8:4-8:8: main
Empieza el programa.

9:3-9:12: int x = 3
Se declara la variable 'x'.

10:13-10:14: x
Se evalúa el primer parámetro.

10:16-10:17: 7
Se evalúa el segundo parámetro.

4:4-4:5: f
Saltamos a la función 'f'.

5:3-5:16: return a * b;
Se retorna 21.

10:11-10:18: f(x, 7)
Se escribe a la salida.
OUTPUT: "21"

10:22-10:26: endl
Se escribe a la salida.
OUTPUT: "
"

11:0-11:1: }
Termina el programa.

