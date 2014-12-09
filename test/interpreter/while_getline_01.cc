#include <iostream>
using namespace std;

int main() {
   string s;
   int n = 0;
   while (getline(cin, s)) {
      n++;
   }
   cout << n << endl;
}
[[in]]---------------------------------------------------------
1
2 2
3 3 3
4 4 4 4
5 5 5 5 5
[[out]]--------------------------------------------------------
5
