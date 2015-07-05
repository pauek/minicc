#include <csignal>
#include <sstream>
#include <fstream>
#include <algorithm>
using namespace std;

#include "parser.hh"
#include "astpr.hh"
#include "prettypr.hh"
#include "interpreter.hh"
#include "translator.hh"
#include "stepper.hh"
#include "semantic.hh"
#include "walker.hh"

// Detect lines like:
//
//   [[out]]-------------------------
//
string test_separator(string line) {
   int p1 = line.find("[[");
   if (p1 != 0) {
      return "";
   }
   int p2 = line.find("]]");
   if (p2 == string::npos) {
      return "";
   }
   int len = line.size() - (p2+2);
   string dashes(len, '-');
   if (line.substr(p2+2) != dashes) {
      return "";
   }
   return line.substr(2, p2-2);
}

string visible_spaces(string output, string compare = "") {
   string res;
   bool first_error_shown = false;
   for (int i = 0; i < output.size(); i++) {
      char c = output[i];
      if (!first_error_shown and compare != "" and output[i] != compare[i]) {
         res += "\x1b[31;1m>";
      }
      switch (c) {
      case ' ': res += '_'; break;
      case '\n': res += "[endl]"; break;
      case '\t': res += "\\t  "; break;
      default:
         res += c;
      }
      if (!first_error_shown and compare != "" and output[i] != compare[i]) {
         res += "<\x1b[0m";
         first_error_shown = true;
      }
      if (c == '\n') {
         res += '\n';
      }
   }
   return res;
}

enum VisitorType { pretty_printer, ast_printer, interpreter, stepper };

void exec_visitor(Program *P, VisitorType vtype) {
}

void parse_test_file(string filename, 
                     string& code, string& in, string& out, string& err) {
   ifstream F(filename);
   string line;
   string *acum = &code;
   while (getline(F, line)) {
      string label = test_separator(line);
      if (label == "") {
         if (line.size() > 0) {
            *acum += line.substr(0, line.size()-1);
            if (line.back() != '\\') {
               *acum += line.back();
               *acum += '\n';
            }
         } else {
            *acum += '\n';
         }
      } else if (label == "out") {
         acum = &out;
      } else if (label == "in") {
         acum = &in;
      } else if (label == "err") {
         acum = &err;
      }
   }
}

void compare_result(string filename, string sout, string serr,
                    string out, string err) {
   char res = '.';
   string sep((82 - filename.size()) / 2, '-');
   string header = sep + " " + filename + " " + sep + "\n";
   if (sout != out) {
      cerr << header << "[out]:" << endl;
      header = "";
      cerr << "target  \"\"\"" << visible_spaces(out, sout) << "\"\"\"" << endl;
      cerr << "current \"\"\"" << visible_spaces(sout, out) << "\"\"\"" << endl;
      cerr << endl;
      res = 'x';
   }
   if (serr != err) {
      cerr << header << "[err]:" << endl;
      cerr << "target  \"\"\"" << visible_spaces(err, serr) << "\"\"\"" << endl;
      cerr << "current \"\"\"" << visible_spaces(serr, err) << "\"\"\"" << endl;
      cerr << endl;
      res = 'x';
   }
   cout << res << flush;
}

int test_visitor(string filename, VisitorType vtype) {
   string code, in, out, err;
   parse_test_file(filename, code, in, out, err);
   
   ostringstream Sout, Saux, Serr;
   istringstream Scode(code), Sin(in);
   Parser P(&Scode, &Serr);
   AstNode *program;
   program = P.parse();
   
   vector<Error*> ve;
   collect_errors(program, ve);
   if (!ve.empty()) {
      for (Error *e : ve) {
         Serr << e->msg << endl;
      }
      goto compare;
   }

   AstVisitor *v;
   switch (vtype) {
   case pretty_printer: v = new PrettyPrinter(&Sout); break;
   case ast_printer:    v = new AstPrinter(&Sout); break;
   case interpreter:    v = new Interpreter(&Sin, &Sout); break;
   default: break;
   }

   // Run it
   try {
      Translator::translator.set_language("es");
      if (vtype == stepper) {
         Stepper S(&Sin, &Saux);
         program->accept(&S);
         while (!S.finished()) {
            Sout << S.span() << ": " << P.input().substr(S.span()) << endl;
            Sout << S.status() << endl;
            string output = S.output();
            if (output != "") {
               Sout << "OUTPUT: \"" << output << '"' << endl;
            }
            Sout << endl;
            if (!S.step()) {
               throw S.error();
            }
         }
      } else {
         program->accept(v);
         collect_errors(program, ve);
         for (Error *e : ve) {
            Serr << e->msg << endl;
         }
      }
   } 
   catch (Error* e) {
      Serr << "Error de ejecución: " << e->msg << endl;
   }

 compare:
   compare_result(filename, Sout.str(), Serr.str(), out, err);
   return 0;
}

int test_semantic(string filename) {
   string code, in, out, err;
   parse_test_file(filename, code, in, out, err);

   ostringstream Sout, Saux, Serr;
   istringstream Scode(code), Sin(in);
   Parser P(&Scode, &Serr);
   AstNode *program;
   program = P.parse();

   SemanticAnalyzer A;
   program->accept(&A);
   vector<Error*> ve;
   collect_errors(program, ve);
   for (Error *e : ve) {
      Serr << filename << "[" << e->ini << "-" << e->fin << "]: " 
           << e->msg << endl;
   }
   compare_result(filename, Sout.str(), Serr.str(), out, err);
   return 0;
}

int test_ast(string filename)   { return test_visitor(filename, ast_printer); }
int test_print(string filename) { return test_visitor(filename, pretty_printer); }
int test_eval(string filename)  { return test_visitor(filename, interpreter); }
int test_step(string filename)  { return test_visitor(filename, stepper); }
