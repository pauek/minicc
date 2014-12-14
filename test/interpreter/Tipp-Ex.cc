#include <iostream>
using namespace std;

int main() {
   string p, L;
   getline(cin, p);
   string q(p.size(), '-');
   while (getline(cin, L)) {
      int pos = L.find(p);
      while (pos != string::npos) {
         L.replace(pos, p.size(), q);
         pos = L.find(p, pos + 1);
      }
      cout << L << endl;
   }
}
[[in]]---------------------------------------------------
jardemor
ksadj lskdf jardemorasd
xxxjardemorxxxxxxxxjardemorxxx
jardemorjardemor_jardemor
[[out]]--------------------------------------------------
ksadj lskdf --------asd
xxx--------xxxxxxxx--------xxx
----------------_--------
