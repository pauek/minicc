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
4:5-4:9: main
Empieza el programa.

5:4-5:13: int i = 0
Se declara la variable 'i'.

6:11-6:16: i < 3
La condición vale 'true', entramos en el while.

7:15-7:16: i
Se escribe a la salida.
OUTPUT: "0"

7:20-7:24: endl
Se escribe a la salida.
OUTPUT: "
"

8:7-8:10: i++
Se incrementa la variable 'i'.

6:11-6:16: i < 3
La condición vale 'true', entramos en el while.

7:15-7:16: i
Se escribe a la salida.
OUTPUT: "1"

7:20-7:24: endl
Se escribe a la salida.
OUTPUT: "
"

8:7-8:10: i++
Se incrementa la variable 'i'.

6:11-6:16: i < 3
La condición vale 'true', entramos en el while.

7:15-7:16: i
Se escribe a la salida.
OUTPUT: "2"

7:20-7:24: endl
Se escribe a la salida.
OUTPUT: "
"

8:7-8:10: i++
Se incrementa la variable 'i'.

6:11-6:16: i < 3
La condición vale 'false', salimos del while.

10:1-10:2: }
Termina el programa.

