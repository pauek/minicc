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
tests/interpreter/ref/ref2.cc[1:0-1:0]: En el parámetro 1 se requiere una variable.
