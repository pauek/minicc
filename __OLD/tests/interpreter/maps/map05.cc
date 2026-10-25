#include <map>
#include <iostream>
using namespace std;

int main() {
   map<int, string> M;
   M[5] = "cinco";
   M[1] = "uno";
   M[2] = "dos";
   M[10] = "diez";
   map<int, string>::iterator it = M.find(3);
   cout << (it == M.end()) << endl;
   it = M.find(10);
   cout << (it == M.end()) << endl;
   cout << it->first << ' ' << it->second << endl;
}
[[out]]--------------------------------------------------
1
0
10 diez
