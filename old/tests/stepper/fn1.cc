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
8:5-8:9: 
Empieza el programa.

9:4-9:13: 
Se declara la variable 'x'.

10:14-10:15: 
Se evalúa el primer parámetro.

10:17-10:18: 
Se evalúa el segundo parámetro.

4:5-4:6: 
Saltamos a la función 'f'.

5:4-5:16: 
Se retorna 21.

10:12-10:19: 
Se escribe a la salida.
OUTPUT: "21"

10:23-10:27: 
Se escribe a la salida.
OUTPUT: "
"

11:1-11:2: 
Termina el programa.

