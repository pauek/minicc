#include <iostream>
using namespace std;

int main() {
   for (int i = 0; i < 3; i++) {
      cout << i << endl;
   }
}
[[out]]--------------------------------------------------
Saltamos a la función 'main'.
5:8-5:18: int i = 0;
Se declara la variable 'i'.
5:19-5:24: i < 3
La condición vale 'true', entramos en el for.
6:14-6:15: i
Se escribe a la salida.
6:19-6:23: endl
Se escribe a la salida.
5:26-5:29: i++
Se incrementa la variable 'i'.
5:19-5:24: i < 3
La condición vale 'true', entramos en el for.
6:14-6:15: i
Se escribe a la salida.
6:19-6:23: endl
Se escribe a la salida.
5:26-5:29: i++
Se incrementa la variable 'i'.
5:19-5:24: i < 3
La condición vale 'true', entramos en el for.
6:14-6:15: i
Se escribe a la salida.
6:19-6:23: endl
Se escribe a la salida.
5:26-5:29: i++
Se incrementa la variable 'i'.
5:19-5:24: i < 3
La condición vale 'false', salimos del for.
[[err]]--------------------------------------------------
