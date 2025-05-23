#include <iostream>
using namespace std;

void incr(int& i) {
   i += 5;
}

int main() {
   int a = 3;
   incr(a);
   cout << a << endl;
}
[[out]]--------------------------------------------------
8
