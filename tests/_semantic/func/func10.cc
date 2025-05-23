#include <iostream>
using namespace std;

void f(int x) {
   cout << x+1 << endl;
}

int main() {
   f(A);
}
[[err]]--------------------------------------------------
tests/semantic/func/func10.cc[9:6-9:7]: No se ha declarado 'A'.
