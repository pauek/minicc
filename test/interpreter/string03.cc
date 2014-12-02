#include <iostream>
using namespace std;

int main() {
   string s = "xxx";
   s.insert(1, "AA");
   cout << s.insert(4, "BB") << endl;
}
[[out]]--------------------------------------------------
xAAxBBx
