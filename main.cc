
#include <iostream>
#include <fstream>
using namespace std;

#include "parser.hh"
#include "test.hh"
#include "astpr.hh"
#include "prettypr.hh"
#include "interpreter.hh"
#include "walker.hh"

int main(int argc, char *argv[]) {
   string filename, todo = "eval";
   if (argc > 1) {
      string argv1 = argv[1];
      if (argv1.substr(0, 7) == "--test-") {
         if (argc < 3) {
            cerr << argv1.substr(2) << ": missing filename" << endl;
            return 1;
         }
         filename = argv[2];
         test(argv1.substr(7), filename);
         return 0;
      } else if (argv1 == "--ast") {
         if (argc >= 3) {
            filename = argv[2];
         }
         todo = "ast";
      } else if (argv1 == "--pprint") {
         if (argc >= 3) {
            filename = argv[2];
         }
         todo = "prettyprint";
      } else {
         filename = argv[1];
      }
   } else {
      cerr << "No has indicado el fichero de código" << endl;
      exit(1);
   }

   ifstream codefile(filename.c_str());
   Parser P(&codefile);
   AstNode *program = P.parse();

   vector<Error*> ve;
   collect_errors(program, ve);
   for (Error *e : ve) {
      cerr << "Error de compilación: " << e->msg << endl;
   }

   AstVisitor *v;
   if (todo == "ast") {
      v = new AstPrinter(&cout);
   } else if (todo == "prettyprint") {
      v = new PrettyPrinter(&cout);
   } else {
      v = new Interpreter(&cin, &cout);
   }

   try {
      program->visit(v);
      collect_errors(program, ve);
      for (Error *e : ve) {
         cerr << e->msg << endl;
      }
      return (ve.empty() ? 0 : 1);
   } 
   catch (EvalError* e) {
      cerr << "Error de ejecución:   " << e->msg << endl;
      return 1;
   }
}
