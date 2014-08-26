#include <iostream>
using namespace std;

int main() {
   int a = 1, b = 2, c = 3;
   cout << (a & b) << ' ' << (a & c) << endl;
   cout << (a | b) << ' ' << (a | c) << endl;
   cout << (a ^ b) << ' ' << (a ^ c) << endl;
   
}
[[out]]--------------------------------------------------
0 1
3 3
3 2
[[err]]--------------------------------------------------
