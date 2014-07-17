
#include <iostream>
#include <fstream>
using namespace std;

#include "parser.hh"
#include "prettyprinter.hh"

int main(int argc, char *argv[]) {
   if (argc > 1 and string(argv[1]) == "--test-parser") {
      if (argc < 3) {
         cerr << "test-parser: missing filename" << endl;
         return 1;
      }
      test_parser(argv[2]);
      return 0;
   }
   istream *i = &cin;
   if (argc > 1) {
      i = new ifstream(argv[1]);
   }
   Parser P(i);
   try {
      AstNode *program = P.parse();
      PrettyPrinter pr(&cout);
      program->visit(&pr);
   } catch (ParseError *e) {
      cerr << e->msg << endl;
   }
}
