#include <map>
#include <iostream>
using namespace std;

int main() {
   map<int, bool> M;
   M[5] = true;
   M[1] = false;
   M[2] = true;
   M[10] = false;
   map<int,bool>::iterator it;
   for (it = M.begin(); it != M.end(); it++) {
      cout << it->first << ' ' << it->second << endl;
   }
}
[[out]]--------------------------------------------------
1 0
2 1
5 1
10 0
