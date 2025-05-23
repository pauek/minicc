#include <iostream>
using namespace std;

struct X {
   int a, b;
};

int main() {
   X c = {1, 2};
   int a = 1;
   a = 3;
   int b = 3;
   b = 7;
   c.a = a;
   c.b = b;
   cout << "Hola, MiniC++" << endl;
}
