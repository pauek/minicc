#include <iostream>
int main() {
   cout << x + 1 << std::endl;
}
[[err]]----------------------------------------------------
tests/semantic/notfound/notfound2.cc[3:4-3:8]: No se ha declarado 'cout'.
tests/semantic/notfound/notfound2.cc[3:12-3:13]: No se ha declarado 'x'.
