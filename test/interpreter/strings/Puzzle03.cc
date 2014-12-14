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
3 d
4 e
0 a
2 c
1 b
-1
[[out]]-------------------------------------------------
"a b c d e"
