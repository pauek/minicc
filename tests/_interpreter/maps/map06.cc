#include <map>
#include <iostream>
using namespace std;

int main() {
   map<int, string> M;
   cout << "empty: " << M.empty() << endl;
   M[5] = "cinco";
   cout << "empty: " << M.empty() << endl;
   M[1] = "uno";
   M.clear();
   cout << "empty: " << M.empty() << endl;
   M[2] = "dos";
   cout << "empty: " << M.empty() << endl;
   M[10] = "diez";
   cout << M.size() << endl;
   map<int, string>::iterator it = M.find(5);
   cout << (it == M.end()) << endl;
}
[[out]]--------------------------------------------------
empty: 1
empty: 0
empty: 1
empty: 0
2
1
