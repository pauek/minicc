#include <iostream>
using namespace std;

void pr(string p, int a, int b) {
   for (int i = a; i < b; i++) {
      cout << p[i];
   }
}

int main() {
   char c;
   string w1, w2;
   cin >> c >> w1 >> w2;
   int p1 = w1.find(c), p2 = w2.find(c);
   pr(w1, 0, p1); 
   pr(w2, p2, w2.size()); 
   cout << endl;
   pr(w2, 0, p2); 
   pr(w1, p1, w1.size());
   cout << endl;
}
[[in]]---------------------------------------------------
- half-assed mid-term
[[out]]--------------------------------------------------
half-term
mid-assed
