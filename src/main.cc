
#include <iostream>
#include <fstream>
#include <map>
using namespace std;

#include "parser.hh"
#include "test.hh"
#include "astpr.hh"
#include "prettypr.hh"
#include "stepper.hh"
#include "semantic.hh"
#include "interpreter.hh"
#include "translator.hh"
#include "walker.hh"

int semantic_analysis(Ast *program, string filename) {
   SemanticAnalyzer A;
   program->accept(&A);
   vector<Error*> ve;
   collect_errors(program, ve);
   for (Error *e : ve) {
      cerr << filename << ":" << e->span.ini << ": " << e->msg << endl;
   }
   return ve.size();
}

void static_analysis(Program *program) {
   // TODO
}

int show_ast(string filename) {
   ifstream codefile(filename);
   Parser P(&codefile);
   Ast *program = P.parse();
   AstVisitor *v = new AstPrinter(&cout);
   program->accept(v);
   vector<Error*> ve;
   collect_errors(program, ve);
   for (Error *e : ve) {
      cerr << e->msg << endl;
   }
   return (ve.empty() ? 0 : 1);
}

int prettyprint(string filename) {
   try {
      ifstream codefile(filename);
      Parser P(&codefile);
      Ast *program = P.parse();
      PrettyPrinter PP(&cout);
      program->accept(&PP);
      vector<Error*> ve;
      collect_errors(program, ve);
      for (Error *e : ve) {
         cerr << e->msg << endl;
      }
      return (ve.empty() ? 0 : 1);
   } 
   catch (Error* e) {
      cerr << _T("Pretty Print Error") << ": " << e->msg << endl;
      return 1;
   }
}

int step(string filename) {
   try {
      ifstream codefile(filename);
      Parser P(&codefile);
      Ast *program = P.parse();

      int nerrors = semantic_analysis(program, filename);
      if (nerrors > 0) {
         return 1;
      }

      Stepper S;
      program->accept(&S);
      while (!S.finished()) {
         cout << S.span() << ": " << P.lexer().SubStr(S.span()) << endl;
         cout << S.status() << endl;
         string out = S.output();
         if (out != "") {
            cout << "OUTPUT: \"" << out << "\"" << endl;
         }
         cout << endl;
         if (!S.step()) {
            throw S.error();
         }
      }
      return 0;
   } 
   catch (Error* e) {
      cerr << _T("Execution Error") << ": " << e->msg << endl;
      return 1;
   }
}

int interpret(string filename) {
   try {
      ifstream codefile(filename);
      Parser P(&codefile);
      Ast *program = P.parse();

      int nerrors = semantic_analysis(program, filename);
      if (nerrors > 0) {
         return 1;
      }

      Interpreter I(&cin, &cout);
      program->accept(&I);
      vector<Error*> ve;
      collect_errors(program, ve);
      for (Error *e : ve) {
         cerr << e->msg << endl;
      }
      return (ve.empty() ? 0 : 1);
   } 
   catch (Error* e) {
      cerr << _T("Execution Error") << ": " << e->msg << endl;
      return 1;
   }
}

int tokenize(string filename) {
   ifstream codefile(filename);
   Lexer L(&codefile);
   L.next();
   L.skip();
   while (!L.end()) {
      Token tok = L.read_token();
      cout << tok.pos << ' ' << L.SubStr(tok) << endl;
      L.skip();
   }
}

string filename, cmd;

typedef int (*CmdFunc)(string);
map<string, CmdFunc> funcs = {
   {"tok",              tokenize},
   {"ast",              show_ast},
   {"pprint",           prettyprint},
   {"step",             step},
   {"eval",             interpret},

   {"test-parser",      test_parser},
   {"test-ast",         test_ast},
   {"test-print",       test_print},
   {"test-semantic",    test_semantic},
   {"test-interpreter", test_eval},
   {"test-stepper",     test_step}
};

void parse_args(int argc, char *argv[]) {
   cmd = "eval";
   if (argc > 1) {
      string argv1 = argv[1];
      if (argv1.substr(0, 2) == "--") {
         if (argc >= 3) {
            filename = argv[2];
         } else {
            cerr << argv1.substr(2) << ": missing filename" << endl;
            exit(1);
         }
         cmd = argv1.substr(2);
      } else {
         filename = argv[1];
      }
   } else {
      cerr << _T("You should specify a filename") << endl;
      exit(1);
   }
}

int main(int argc, char *argv[]) {
   Translator::translator.set_language("es");
   parse_args(argc, argv);
   (*funcs[cmd])(filename);
}
