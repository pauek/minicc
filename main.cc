
#include <iostream>
#include <fstream>
using namespace std;

#include "parser.h"

int main(int argc, char *argv[]) {
   istream *i = &cin;
   if (argc > 1) {
      i = new ifstream(argv[1]);
   }
   Parser P(i);
   AstNode *program = P.parse();
}
