#include <iostream>
using namespace std;

void incr(int& i) {
   i += 5;
}

int main() {
   incr(1);
}
[[out]]--------------------------------------------------
[[err]]--------------------------------------------------
Error de ejecución: En el parámetro 1 se requiere una variable.
