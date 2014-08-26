#include <iostream>
using namespace std;

int main() {
   int ia = 2, ib = 3;
   ia += 1;
   ib += ia;
   cout << ia << ' ' << ib << endl;

   float fa = 0.2, fb = 0.3;
   fa += 0.1;
   fb += fa;
   cout << fa << ' ' << fb << endl;

   double da = 0.2, db = 0.3;
   da += 0.1;
   db += da;
   cout << da << ' ' << db << endl;

   string sa = "abc", sb = "cde";
   sa += "__";
   sb += sa;
   cout << sa << ' ' << sb << endl;

   int ma = 7, mb = 5;
   ma %= mb;
   cout << ma << ' ' << mb << endl;
   
}
[[out]]--------------------------------------------------
3 6
0.3 0.6
0.3 0.6
abc__ cdeabc__
2 5
[[err]]--------------------------------------------------
