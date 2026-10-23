#include <map>
#include <iostream>
using namespace std;

int main() {
   map<int, string> M;
   M[1] = "hi";
   M[2] = "ho";
   M[3] = "bla";
   M[3] += "bla";
   map<int, string>::iterator it = M.begin();
   M.erase(it);
   cout << "size: " << M.size() << endl;
   for (it = M.begin(); it != M.end(); it++) {
      cout << it->first << ' ' << '"' << it->second << '"' << endl;
   }
   cout << "size: " << M.size() << endl;
   for (int i = 1; i <= 3; i++) {
      cout << i << ' ' << '"' << M[i] << '"' << endl;
   }
   cout << "size: " << M.size() << endl;
}
[[out]]--------------------------------------------------
size: 2
2 "ho"
3 "blabla"
size: 2
1 ""
2 "ho"
3 "blabla"
size: 3
