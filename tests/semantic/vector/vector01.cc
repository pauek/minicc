#include <vector>
#include <iostream>
using namespace std;

double f() {
   return 3.1;
}

int main() {
   vector<int> a(3, -2);
   cout << a[f()] << endl;
}
[[err]]--------------------------------------------------
tests/semantic/vector/vector01.cc[11:14-11:17]: El índice a una casilla de un vector debe ser un 'int' (no 'double').
