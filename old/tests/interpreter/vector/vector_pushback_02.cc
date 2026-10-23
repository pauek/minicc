#include <iostream>
#include <vector>
using namespace std;

int main() {
   vector<string> X(3, "blah");
   X.push_back("hola");
   cout << X.size() << ' ' << X[0] << ' ' << X[3] << endl;
}
[[out]]--------------------------------------------------
4 blah hola
