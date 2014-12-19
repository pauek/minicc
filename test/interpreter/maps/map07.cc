#include <map>
#include <iostream>
using namespace std;

int main() {
   map<int, int> M;
   M[1]++;
   M[2]++;
   M[3]++;
   M[3]++;
   M.erase(2);
   cout << M.size() << endl;
   cout << M[1] << M[2] << M[3] << endl;
   cout << M.size() << endl;
}
[[out]]--------------------------------------------------
2
102
3
