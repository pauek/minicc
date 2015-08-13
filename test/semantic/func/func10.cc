#include <iostream>
using namespace std;

void f(int x) {
   cout << x+1 << endl;
}

int main() {
   f(A);
}
[[err]]--------------------------------------------------
semantic/func/func10.cc[9:5-9:6]: No se ha declarado 'A'.
