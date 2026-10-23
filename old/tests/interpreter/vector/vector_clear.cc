#include <iostream>
#include <vector>
using namespace std;

int main() {
   vector<string> Y(3, "hola");
   for (int i = 0; i < Y.size(); i++) {
      cout << Y[i] << ' ';
   }
   cout << endl;
   Y.clear();
   cout << Y.size() << endl;
}
[[out]]--------------------------------------------------
hola hola hola 
0
