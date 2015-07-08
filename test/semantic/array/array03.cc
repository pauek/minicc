#include <iostream>
using namespace std;

int main() {
   string a[3];
   a[2.1] = "hi";
}
[[err]]--------------------------------------------------
semantic/array/array03.cc[6:3-6:9]: El índice debe ser un entero.
