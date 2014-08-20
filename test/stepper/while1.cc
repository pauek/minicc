#include <iostream>
using namespace std;

int main() {
   int i = 0;
   while (i < 3) {
      cout << i << endl;
      i++;
   }
}
[[out]]--------------------------------------------------
Saltamos a la función 'main'.
5:3-5:13: int i = 0;
Se declara la variable 'i'.
6:10-6:15: i < 3
La condición vale 'true', entramos en el while.
7:6-7:24: cout << i << endl;
Se escribe a la salida.
8:6-8:10: i++;
Se incrementa la variable 'i'.
6:10-6:15: i < 3
La condición vale 'true', entramos en el while.
7:6-7:24: cout << i << endl;
Se escribe a la salida.
8:6-8:10: i++;
Se incrementa la variable 'i'.
6:10-6:15: i < 3
La condición vale 'true', entramos en el while.
7:6-7:24: cout << i << endl;
Se escribe a la salida.
8:6-8:10: i++;
Se incrementa la variable 'i'.
6:10-6:15: i < 3
La condición vale 'false', salimos del while.
[[err]]--------------------------------------------------
