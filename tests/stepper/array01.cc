#include <iostream>
using namespace std;

int main() {
   int a[2] = {-5, -15}; 
   cout << a[0] << endl;
   cout << a[1] << endl;
}

[[out]]--------------------------------------------------
4:5-4:9: main
Empieza el programa.

5:4-5:24: int a[2] = {-5, -15}
Se declara la variable 'a'.

6:12-6:16: a[0]
Se escribe a la salida.
OUTPUT: "-5"

6:20-6:24: endl
Se escribe a la salida.
OUTPUT: "
"

7:12-7:16: a[1]
Se escribe a la salida.
OUTPUT: "-15"

7:20-7:24: endl
Se escribe a la salida.
OUTPUT: "
"

8:1-8:2: }
Termina el programa.

