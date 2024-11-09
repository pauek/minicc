#include <vector>
#include <iostream>
using namespace std;

int main() {
   vector<int> a(3, -2);
   a[] = 4;
}
[[err]]--------------------------------------------------
tests/semantic/vector/vector02.cc[7:6]: Debe haber una expresión entre los corchetes.
