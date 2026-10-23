#include <iostream>
int main() {
   int x = 1;
   std::cout << x + 1 << std::end1;
}
[[err]]----------------------------------------------------
tests/semantic/notfound/notfound1.cc[4:26-4:35]: No se ha encontrado 'end1' en el namespace 'std'.
