#include <iostream>
using namespace std;

int main() {
   cout << "hello" << ", there" << endl;
}
[[out]]--------------------------------------------------
4:5-4:9: main
Empieza el programa.

5:12-5:19: "hello"
Se escribe a la salida.
OUTPUT: "hello"

5:23-5:32: ", there"
Se escribe a la salida.
OUTPUT: ", there"

5:36-5:40: endl
Se escribe a la salida.
OUTPUT: "
"

6:1-6:2: }
Termina el programa.

