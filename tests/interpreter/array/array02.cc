#include <iostream>
using namespace std;

int main() {
   int a[3][2];
   a[0][0] = 1; a[0][1] = 2;
   a[1][0] = 3; a[1][1] = 4;
   a[2][0] = 5; a[2][1] = 6;
   for (int i = 0; i < 3; i++) {
      for (int j = 0; j < 2; j++) {
         cout << a[i][j] << ' ';
      }
      cout << endl;
   }
}
[[out]]--------------------------------------------------
1 2 
3 4 
5 6 
