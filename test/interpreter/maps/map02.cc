#include <map>
#include <iostream>
using namespace std;

int main() {
   map<int, bool> M;
   pair<int, bool> p = {5, true};
   M.insert(p);
   cout << M.size() << endl;
}
[[out]]--------------------------------------------------
1
