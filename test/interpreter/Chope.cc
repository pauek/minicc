#include <iostream>
using namespace std;

int main() {
   string L;
   int n;
   cin >> n;
   getline(cin, L);
   getline(cin, L);
   while (L != "") {
      cout << L.substr(n, n) << L.substr(0, n);
      L = L.substr(2 * n);
   }
   cout << endl;
}
[[in]]--------------------------------------------------
2
dotoenbi
[[out]]-------------------------------------------------
todobien
