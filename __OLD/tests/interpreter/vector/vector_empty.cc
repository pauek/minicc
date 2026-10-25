#include <iostream>
#include <vector>
using namespace std;

int main() {
   vector<bool> A, B(2), C, D;
   D.resize(2);
   cout << A.empty() << B.empty() << C.empty() << D.empty() << endl;
}
[[out]]--------------------------------------------------
1010
