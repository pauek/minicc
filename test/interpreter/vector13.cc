#include <iostream>
#include <vector>
using namespace std;

int main() {
   vector<int> X;
   X.resize(4);
   X.resize(6, 17);
   cout << X.size() << endl;
   for (int i = 0; i < X.size(); i++) {
      cout << X[i] << ' ';
   }
   cout << endl;
}
[[out]]--------------------------------------------------
6
0 0 0 0 17 17 
