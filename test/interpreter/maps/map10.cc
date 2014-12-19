#include <map>
#include <iostream>
using namespace std;

int main() {
   map<int, string> M;
   M[1] = "hi";
   M[2] = "ho";
   M[3] = "bla";
   M[4] = "si";
   map<int, string>::iterator it = M.begin();
   it++;
   M.erase(it);
   cout << M[1] << M[2] << M[3] << M[4] << endl;
}
[[out]]--------------------------------------------------
hiblasi
