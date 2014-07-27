
#include <iostream>
#include <fstream>
using namespace std;

#include "parser.hh"
#include "test.hh"
#include "astpr.hh"
#include "prettypr.hh"
#include "walker.hh"

int main(int argc, char *argv[]) {
   string filename;
   bool print_ast = false;
   if (argc > 1) {
      if (string(argv[1]) == "--test-print") {
         if (argc < 3) {
            cerr << "test-print: missing filename" << endl;
            return 1;
         }
         test_parser(argv[2]);
         return 0;
      } 
      if (string(argv[1]) == "--test-ast") {
         if (argc < 3) {
            cerr << "test-ast: missing filename" << endl;
            return 1;
         }
         test_ast(argv[2]);
         return 0;
      } 
      else if (string(argv[1]) == "--ast") {
         if (argc >= 3) {
            filename = argv[2];
         }
         print_ast = true;         
      } else {
         filename = argv[1];
      }
   }

   istream *i = &cin;
   if (filename != "") {
      i = new ifstream(filename.c_str());
   }
   Parser P(i);
   AstNode *program = P.parse();
   AstVisitor *v;
   if (print_ast) {
      v = new AstPrinter(&cout);
   } else {
      v = new PrettyPrinter(&cout);
   }
   program->visit(v);
   vector<Error*> ve;
   collect_errors(program, ve);
   for (Error *e : ve) {
      cerr << e->msg << endl;
   }
}
