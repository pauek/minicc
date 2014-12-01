#include <iostream>
#include <vector>
using namespace std;

int main() {
   vector<int> X(4, 13);
   X.resize(6, 17);
   cout << X.size() << endl;
   for (int i = 0; i < X.size(); i++) {
      cout << X[i] << ' ';
   }
   cout << endl;
}
[[out]]--------------------------------------------------
6
13 13 13 13 17 17 
