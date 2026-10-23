#include <iostream>
#include <vector>
using namespace std;

int main() {
   vector<int> X;
   vector<string> Y(3, "hola");
   X.resize(3);
   for (int i = 0; i < X.size(); i++) {
      cout << X[i] << ' ';
   }
   cout << endl;
   for (int i = 0; i < Y.size(); i++) {
      cout << Y[i] << ' ';
   }
   cout << endl;
}
[[out]]--------------------------------------------------
0 0 0 
hola hola hola 
