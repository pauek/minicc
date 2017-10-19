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
8:5-8:9: main
Empieza el programa.

9:4-9:13: int x = 3
Se declara la variable 'x'.

10:14-10:15: x
Se evalúa el primer parámetro.

10:17-10:18: 7
Se evalúa el segundo parámetro.

4:5-4:6: f
Saltamos a la función 'f'.

5:4-5:16: return a * b
Se retorna 21.

10:12-10:19: f(x, 7)
Se escribe a la salida.
OUTPUT: "21"

10:23-10:27: endl
Se escribe a la salida.
OUTPUT: "
"

11:1-11:2: }
Termina el programa.

