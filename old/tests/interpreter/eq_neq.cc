#include <iostream>
using namespace std;

int main() {
   int a = 1, b = 1, c = 2;
   cout << (a == b) << ' ';
   cout << (a != b) << ' ';
   cout << (a == c) << ' ';
   cout << (a != c) << endl;

   float fa = 1.5, fb = 1.5, fc = 2.55;
   cout << (fa == fb) << ' ';
   cout << (fa != fb) << ' ';
   cout << (fa == fc) << ' ';
   cout << (fa != fc) << endl;

   double da = 1.5, db = 1.5, dc = 2.55;
   cout << (da == db) << ' ';
   cout << (da != db) << ' ';
   cout << (da == dc) << ' ';
   cout << (da != dc) << endl;
   
}
[[out]]--------------------------------------------------
1 0 0 1
1 0 0 1
1 0 0 1
