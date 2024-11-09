#include <iostream>
using namespace std;

const int N = 5;

int main() {
   int a[N];
   a[4] = 2;
   a[0] = -1;
   cout << a[0] << ' ' << a[4] << endl;
}
[[out]]--------------------------------------------------
-1 2
