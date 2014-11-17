#include <iostream>
using namespace std;

void inc(int& x, int y) {
   x += y;
}

int main() {
   vector<int> a(5);
   inc(a[1], 3);
   a[2] = 10;
   for (int i = 0; i < 5; i++) {
      cout << a[i] << ' ';
   }
   cout << endl;
}
[[out]]--------------------------------------------------
0 3 10 0 0 
