#include <sstream>
#include <iostream>
using namespace std;

int main() {
   ostringstream sout;
   sout << "hola" << 1 << 'c';
   cout << sout.str() << endl;
}
[[out]]--------------------------------------------------
hola1c
