#include <iostream>
using namespace std;

int main() {
   int n, M = 0;
   string p, F[20];
   cin >> n; 
   while (n != -1) {
      if (n > M) M = n;
      getline(cin, p);
      F[n] = p;
      cin >> n;
   }
   F[0][0] = '"';
   for (int i = 0; i <= M; i++) {
      cout << F[i];
   }
   cout << '"' << endl;
}
[[in]]--------------------------------------------------
3 clavito.
1 clavó
0 Pablito
2 un
-1
[[out]]-------------------------------------------------
"Pablito clavó un clavito."
