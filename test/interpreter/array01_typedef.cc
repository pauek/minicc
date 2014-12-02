#include <iostream>
using namespace std;

typedef int Tab3[3];

int main() {
   Tab3 a = { 142, 98, 57 };
   a[1]++;
   a[2]--;
   cout << a[0] << ' ' << a[1] << ' ' << a[2] << endl;
}
[[out]]--------------------------------------------------
142 99 56
