#include <iostream>
using namespace std;

int main() {
   string a[3];
   a[2.1] = "hi";
}
[[err]]--------------------------------------------------
tests/semantic/array/array03.cc[6:4-6:10]: El índice debe ser un entero.
