
#include <iostream>
#include <fstream>
using namespace std;

#include "input.h"

int main(int argc, char *argv[]) {
   istream *i = &cin;
   if (argc > 1) {
      i = new ifstream(argv[1]);
   }
   Input I(i);
   while (I.next()) {
      cout << "'" << I.curr() << "' " << I.pos() << endl;
   }
}
