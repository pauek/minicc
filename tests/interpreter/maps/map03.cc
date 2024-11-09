#include <map>
#include <iostream>
using namespace std;

int main() {
   map<int, bool> M;
   M[5] = true;
   cout << M.size() << endl;
}
[[out]]--------------------------------------------------
1
