#include <iostream>
using namespace std;

int main() {
   int a = 3;
   int& ra = a;
   ra++;
   cout << a << endl;
}
[[out]]--------------------------------------------------
4
