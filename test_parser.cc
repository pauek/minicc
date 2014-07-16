#include <sstream>
#include <fstream>
using namespace std;

#include "parser.hh"
#include "prettyprint.hh"

// Detect lines like:
//
//   [[out]]-------------------------
//
string test_parser_separator(string line) {
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

string visible_spaces(string output) {
   string res;
   for (char c : output) {
      switch (c) {
      case ' ': res += '_'; break;
      case '\n': res += "[endl]\n"; break;
      case '\t': res += "\\t  "; break;
      default:
         res += c;
      }
   }
   return res;
}

void test_parser(string filename) {
   ifstream F(filename);
   string line, code, out, err;

   string *acum = &code;
   while (getline(F, line)) {
      string label = test_parser_separator(line);
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
      PrettyPrinter pr(&Sout);
      program->visit(&pr);
   } catch (ParseError *e) {
      Serr << e->msg << endl;
   }
   char res = '.';
   string sep((82 - filename.size()) / 2, '-');
   string header = sep + " " + filename + " " + sep + "\n";
   if (Sout.str() != out) {
      cerr << header << "[out]:" << endl;
      header = "";
      cerr << "target  \"\"\"" << visible_spaces(out) << "\"\"\"" << endl;
      cerr << "current \"\"\"" << visible_spaces(Sout.str()) << "\"\"\"" << endl;
      cerr << endl;
      res = 'x';
   }
   if (Serr.str() != err) {
      cerr << header << "[err]:" << endl;
      cerr << "target  \"\"\"" << visible_spaces(err) << "\"\"\"" << endl;
      cerr << "current \"\"\"" << visible_spaces(Serr.str()) << "\"\"\"" << endl;
      cerr << endl;
      res = 'x';
   }
   cout << res << flush;
}
