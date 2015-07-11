#include <iostream>

int main() {
   std::cout << 1 << std::end1;
}

[[err]]----------------------------------------------------
semantic/namespace/notfound1.cc[4:21-4:30]: No se ha encontrado 'end1' en el namespace 'std'.
