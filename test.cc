#include <sstream>
#include <fstream>
using namespace std;

#include "parser.hh"
#include "astprinter.hh"
#include "prettyprinter.hh"

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
      case '\n': res += "[endl]\n"; break;
      case '\t': res += "\\t  "; break;
      default:
         res += c;
      }
      if (!first_error_shown and compare != "" and output[i] != compare[i]) {
         res += "<\x1b[0m";
         first_error_shown = true;
      }
   }
   return res;
}

enum VisitorType { pretty_printer, ast_printer };

void test_visitor(string filename, VisitorType vtype) {
   ifstream F(filename);
   string line, code, out, err;

   string *acum = &code;
   while (getline(F, line)) {
      string label = test_separator(line);
      if (label == "") {
         *acum += line + "\n";
      } else if (label == "out") {
         acum = &out;
      } else if (label == "err") {
         acum = &err;
      }
   }
   
   ostringstream Sout, Serr;
   istringstream Scode(code);
   Parser P(&Scode, &Serr);
   AstNode *program;
   try {
      program = P.parse();
      AstVisitor *v;
      switch (vtype) {
      case pretty_printer: v = new PrettyPrinter(&Sout); break;
      case ast_printer:    v = new AstPrinter(&Sout); break;
      }
      program->visit(v);
   } 
   catch (ParseError *e) {
      Serr << e->msg << endl;
   }
   char res = '.';
   string sep((82 - filename.size()) / 2, '-');
   string header = sep + " " + filename + " " + sep + "\n";
   if (Sout.str() != out) {
      cerr << header << "[out]:" << endl;
      header = "";
      cerr << "target  \"\"\"" << visible_spaces(out) << "\"\"\"" << endl;
      cerr << "current \"\"\"" << visible_spaces(Sout.str(), out) << "\"\"\"" << endl;
      cerr << endl;
      res = 'x';
   }
   if (Serr.str() != err) {
      cerr << header << "[err]:" << endl;
      cerr << "target  \"\"\"" << visible_spaces(err) << "\"\"\"" << endl;
      cerr << "current \"\"\"" << visible_spaces(Serr.str(), err) << "\"\"\"" << endl;
      cerr << endl;
      res = 'x';
   }
   cout << res << flush;
}

void test_parser(string filename) { test_visitor(filename, pretty_printer); }
void test_ast   (string filename) { test_visitor(filename,    ast_printer); }
