#include <iostream>
using namespace std;

int main() {
   for (int i = 0; i < 3; i++) {
      cout << i << endl;
   }
}
[[out]]--------------------------------------------------
4:5-4:9: 
Empieza el programa.

5:9-5:18: 
Se declara la variable 'i'.

5:20-5:25: 
La condición vale 'true', entramos en el for.

6:15-6:16: 
Se escribe a la salida.
OUTPUT: "0"

6:20-6:24: 
Se escribe a la salida.
OUTPUT: "
"

5:27-5:30: 
Se incrementa la variable 'i'.

5:20-5:25: 
La condición vale 'true', entramos en el for.

6:15-6:16: 
Se escribe a la salida.
OUTPUT: "1"

6:20-6:24: 
Se escribe a la salida.
OUTPUT: "
"

5:27-5:30: 
Se incrementa la variable 'i'.

5:20-5:25: 
La condición vale 'true', entramos en el for.

6:15-6:16: 
Se escribe a la salida.
OUTPUT: "2"

6:20-6:24: 
Se escribe a la salida.
OUTPUT: "
"

5:27-5:30: 
Se incrementa la variable 'i'.

5:20-5:25: 
La condición vale 'false', salimos del for.

8:1-8:2: 
Termina el programa.

