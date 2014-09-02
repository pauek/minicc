#include <iostream>
using namespace std;

int main() {
   for (int i = 0; i < 3; i++) {
      cout << i << endl;
   }
}
[[out]]--------------------------------------------------
4:4-4:8: main
Empieza el programa.

5:8-5:18: int i = 0;
Se declara la variable 'i'.

5:19-5:24: i < 3
La condición vale 'true', entramos en el for.

6:14-6:15: i
Se escribe a la salida.
OUTPUT: "0"

6:19-6:23: endl
Se escribe a la salida.
OUTPUT: "
"

5:26-5:29: i++
Se incrementa la variable 'i'.

5:19-5:24: i < 3
La condición vale 'true', entramos en el for.

6:14-6:15: i
Se escribe a la salida.
OUTPUT: "1"

6:19-6:23: endl
Se escribe a la salida.
OUTPUT: "
"

5:26-5:29: i++
Se incrementa la variable 'i'.

5:19-5:24: i < 3
La condición vale 'true', entramos en el for.

6:14-6:15: i
Se escribe a la salida.
OUTPUT: "2"

6:19-6:23: endl
Se escribe a la salida.
OUTPUT: "
"

5:26-5:29: i++
Se incrementa la variable 'i'.

5:19-5:24: i < 3
La condición vale 'false', salimos del for.

8:0-8:1: }
Termina el programa.

[[err]]--------------------------------------------------
