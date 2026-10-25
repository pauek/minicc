
#include <iostream>
using namespace std;

int main() {
   string p;
   int tam[4] = {-1, 0, 0, 0};
   while (cin >> p) {
      if (p.size() <= 3) {
         tam[p.size()]++;
      }
   }
   for (int i = 1; i <= 3; i++) {
      cout << "De " << i << " lletres: " << tam[i] << endl;
   }
}
[[in]]------------------------------------------------------
A mi si que re m'hi fa que faci sol
[[out]]-----------------------------------------------------
De 1 lletres: 1
De 2 lletres: 4
De 3 lletres: 3
