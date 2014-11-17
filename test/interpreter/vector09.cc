#include <iostream>
using namespace std;

int main() {
   vector<string> X(4);
   X.resize(3);
   cout << X.size() << endl;
}
[[out]]--------------------------------------------------
3
