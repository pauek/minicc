#include <iostream>
using namespace std;

int main() {
   int a[3] = { 142, 98, 57 };
   a[1]++;
   a[2]--;
   cout << a[0] << ' ' << a[1] << ' ' << a[2] << endl;
}
[[out]]--------------------------------------------------
142 99 56
