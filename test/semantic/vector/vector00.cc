#include <vector>
#include <iostream>
using namespace std;

int main() {
   vector<int> a(3, -2);
   a[0] = "hola";
}
[[err]]--------------------------------------------------
semantic/vector/vector00.cc[7:3-7:16]: No se puede asignar un 'string' a una variable de tipo 'int'.
