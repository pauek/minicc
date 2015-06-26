#include <iostream>
using namespace std;

int main() {
   int a[2] = {-5, -15}; 
   cout << a[0] << endl;
   cout << a[1] << endl;
}

[[out]]--------------------------------------------------
4:4-4:8: main
Empieza el programa.

5:3-5:23: int a[2] = {-5, -15}
Se declara la variable 'a'.

6:11-6:15: a[0]
Se escribe a la salida.
OUTPUT: "-5"

6:19-6:23: endl
Se escribe a la salida.
OUTPUT: "
"

7:11-7:15: a[1]
Se escribe a la salida.
OUTPUT: "-15"

7:19-7:23: endl
Se escribe a la salida.
OUTPUT: "
"

8:0-8:1: }
Termina el programa.

