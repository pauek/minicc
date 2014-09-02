#include <iostream>
using namespace std;

int main() {
   cout << "hello" << ", there" << endl;
}
[[out]]--------------------------------------------------
4:4-4:8: main
Empieza el programa.

5:11-5:18: "hello"
Se escribe a la salida.
OUTPUT: "hello"

5:22-5:31: ", there"
Se escribe a la salida.
OUTPUT: ", there"

5:35-5:39: endl
Se escribe a la salida.
OUTPUT: "
"

6:0-6:1: }
Termina el programa.

