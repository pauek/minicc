#include <iostream>

int main() {
   std::cout << 1 << std::end1;
}

[[err]]----------------------------------------------------
tests/semantic/namespace/notfound1.cc[4:22-4:31]: No se ha encontrado 'end1' en el namespace 'std'.
