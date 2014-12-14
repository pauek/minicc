#include <iostream>
using namespace std;

int main() {
   char x;
   cin >> x;
   string p1, p2;
   cin >> p1 >> p2;
   int a = p1.find(x), b = p2.find(x);
   cout << p1.substr(0, a) << p2.substr(b) << endl;
   cout << p2.substr(0, b) << p1.substr(a) << endl;
}
[[in]]---------------------------------------------------
/ in/out red/black
[[out]]--------------------------------------------------
in/black
red/out
