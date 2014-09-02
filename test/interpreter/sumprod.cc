#include <iostream>
using namespace std;

int main() {
   int a = 1, b = 2;
   cout << (a + b) << ' ' << (a - b) << ' ';
   cout << (a * b) << ' ' << (a / b) << ' ';
   cout << (a % b) << endl;

   float fa = 2.3, fb = 0.6;
   cout << (fa + fb) << ' ' << (fa - fb) << ' ';
   cout << (fa * fb) << ' ' << (fa / fb) << endl;

   double da = 2.3, db = 0.6;
   cout << (da + db) << ' ' << (da - db) << ' ';
   cout << (da * db) << ' ' << (da / db) << endl;

   string sa = "abc", sb = "def";
   cout << (sa + sb) << endl;
}
[[out]]--------------------------------------------------
3 -1 2 0 1
2.9 1.7 1.38 3.83333
2.9 1.7 1.38 3.83333
abcdef
