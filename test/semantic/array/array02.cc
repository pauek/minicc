#include <iostream>
using namespace std;

int main() {
   int a[3];
   cout << a[3] << endl;
   cout << a[-1] << endl;
}
[[err]]--------------------------------------------------
semantic/array/array02.cc[6:12-6:16]: El índice está fuera de los límites de la tabla (entre 0 y 2).
semantic/array/array02.cc[7:12-7:17]: El índice está fuera de los límites de la tabla (entre 0 y 2).
