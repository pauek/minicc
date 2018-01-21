
#include <iostream>
#include <fstream>
#include <map>
using namespace std;

#include "parser.hh"
#include "astpr.hh"
#include "prettypr.hh"
#include "stepper.hh"
#include "semantic.hh"
#include "interpreter.hh"
#include "translator.hh"
#include "walker.hh"

#include "new_types.hh"
#include "new_vm.hh"

int test_vm(string filename) {
   using vm::TypeDescription;
   using vm::TypeTable;
   using vm::NameTable;
   using vm::type_index_t;
   using vm::mem_index_t;
   using vm::Struct;
   using vm::Array;
   using vm::Composite;
   using vm::StructDescription;
   using vm::ArrayDescription;
   using vm::I32;
   using vm::F32;
   using vm::F64;
   using vm::Memory;
   using vm::MiB;

   TypeTable tab;
   NameTable names;

   cout << tab.SizeOf(vm::I32) << endl;
   type_index_t point2d = tab.Add(new StructDescription(names.Put("Point2D"), {
      { names.Put("x"), F64 },
      { names.Put("y"), F64 }
   }));
   vm::Type point2d_t = vm::Type(Composite, point2d);
   cout << tab.SizeOf(point2d_t) << endl;

   type_index_t pointtab = tab.Add(new ArrayDescription(
      names.Put("Vector10"),
      (size_t)12,
      point2d_t
   ));
   vm::Type pointtab_t(Composite, pointtab);
   cout << tab.SizeOf(pointtab_t) << endl;

   vm::Type i32(I32);

   Memory M(8 * MiB, tab);
   mem_index_t chunk1;
   if (M.Alloc(pointtab_t, chunk1)) {
      cout << "alloc: " << chunk1 << " (" << M.Get(chunk1)->start << ")" << endl;
   }
   mem_index_t chunk2;
   if (M.Alloc(i32, chunk2)) {
      cout << "alloc: " << chunk2 << " (" << M.Get(chunk2)->start << ")" << endl;
   }
   M.Free(chunk1);
   mem_index_t chunk3;
   if (M.Alloc(i32, chunk3)) {
      cout << "alloc: " << chunk3 << " (" << M.Get(chunk3)->start << ")" << endl;
   }
   mem_index_t chunk4;
   if (M.Alloc(i32, chunk4)) {
      cout << "alloc: " << chunk4 << " (" << M.Get(chunk4)->start << ")" << endl;
   }
   return 0;
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
   return 0;
}

int show_ast(string filename) {
   ifstream codefile(filename);
   Parser P(&codefile);
   Ast *program = P.parse();
   AstPrint(program);
   return 0;
}

int prettyprint(string filename) {
   try {
      ifstream codefile(filename);
      Parser P(&codefile);
      Ast *program = P.parse();
      PrettyPrint(program);
   } 
   catch (Error* e) {
      cerr << _T("Pretty Print Error") << ": " << e->msg << endl;
      return 1;
   }
   return 0;
}

int semantic_analysis(Ast *program, string filename) {
   AnalyzeSemantics(program);
   vector<Error*> ve;
   collect_errors(program, ve);
   for (Error *e : ve) {
      cerr << filename << ":" << e->span.begin << ": " << e->msg << endl;
   }
   return ve.size();
}

Ast *parse_and_analyze(string filename) {
   ifstream codefile(filename);
   Parser P(&codefile);
   Ast *program = P.parse();

   int nerrors = semantic_analysis(program, filename);
   if (nerrors > 0) {
      exit(1);
   }
   return program;
}

int interpret(string filename) {
   try {
      Ast *program = parse_and_analyze(filename);
      Eval(program, cin, cout);
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
   return 0;
}

int step(string filename) {
   try {
      ifstream codefile(filename);
      Parser P(&codefile);
      Ast *program = P.parse();

      int nerrors = semantic_analysis(program, filename);
      if (nerrors > 0) {
         exit(1);
      }
      Stepper S;
      S.Step(program);
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

int _test_parser_and_semantic(string filename, bool do_semantic) {
   string code, in, out, err;
   parse_test_file(filename, code, in, out, err);

   ostringstream Sout, Saux, Serr;
   istringstream Scode(code), Sin(in);
   Parser P(&Scode, &Serr);
   Ast *program;
   try {
      program = P.parse();
      if (do_semantic) {
         AnalyzeSemantics(program);
      }
      vector<Error*> ve;
      collect_errors(program, ve);
      for (Error *e : ve) {
         Serr << filename << "[" << e->span.begin << "-" << e->span.end << "]: " 
              << e->msg << endl;
      }
   }
   catch (ParseError& e) {
      Serr << filename << "[" << e.pos << "]: " << e.msg << endl;
   }
   compare_result(filename, Sout.str(), Serr.str(), out, err);
   return 0;
}

int test_semantic(string filename) {
   return _test_parser_and_semantic(filename, true);
}

int test_parser(string filename) {
   return _test_parser_and_semantic(filename, false);
}

int test_ast(string filename) {
   string code, in, out, err;
   parse_test_file(filename, code, in, out, err);
   
   ostringstream Sout, Saux, Serr;
   istringstream Scode(code), Sin(in);
   Parser P(&Scode, &Serr);
   Ast *program;
   try {
      program = P.parse();
   }
   catch (ParseError& e) {
      Serr << filename << "[" << e.pos << "]: " << e.msg << endl;
      goto compare;
   }

   try {
      Translator::translator.set_language("es");
      vector<Error*> ve;
      AstPrint(program, Sout);
      collect_errors(program, ve);
      for (Error *e : ve) {
         Serr << e->msg << endl;
      }
   } 
   catch (Error* e) {
      Serr << "Error de ejecución: " << e->msg << endl;
   }

 compare:
   compare_result(filename, Sout.str(), Serr.str(), out, err);
   return 0;
}

int test_print(string filename) {
   string code, in, out, err;
   parse_test_file(filename, code, in, out, err);
   
   ostringstream Sout, Saux, Serr;
   istringstream Scode(code), Sin(in);
   Parser P(&Scode, &Serr);
   Ast *program;
   try {
      program = P.parse();
   }
   catch (ParseError& e) {
      Serr << filename << "[" << e.pos << "]: " << e.msg << endl;
      goto compare;
   }
   try {
      Translator::translator.set_language("es");
      vector<Error*> ve;
      PrettyPrint(program, Sout);
      collect_errors(program, ve);
      for (Error *e : ve) {
         Serr << e->msg << endl;
      }
   } 
   catch (Error* e) {
      Serr << "Error de ejecución: " << e->msg << endl;
   }

 compare:
   compare_result(filename, Sout.str(), Serr.str(), out, err);
   return 0;
}

int test_eval(string filename) {
   string code, in, out, err;
   parse_test_file(filename, code, in, out, err);
   
   ostringstream Sout, Saux, Serr;
   istringstream Scode(code), Sin(in);
   Parser P(&Scode, &Serr);
   Ast *program;
   try {
      program = P.parse();
   }
   catch (ParseError& e) {
      Serr << filename << "[" << e.pos << "]: " << e.msg << endl;
      goto compare;
   }
   try {
      Translator::translator.set_language("es");
      vector<Error*> ve;
      Eval(program, Sin, Sout);
      collect_errors(program, ve);
      for (Error *e : ve) {
         Serr << e->msg << endl;
      }
   } 
   catch (Error* e) {
      Serr << "Error de ejecución: " << e->msg << endl;
   }

 compare:
   compare_result(filename, Sout.str(), Serr.str(), out, err);
   return 0;
}

int test_step(string filename) {
   string code, in, out, err;
   parse_test_file(filename, code, in, out, err);
   
   ostringstream Sout, Saux, Serr;
   istringstream Scode(code), Sin(in);
   Parser P(&Scode, &Serr);
   Ast *program;
   try {
      program = P.parse();
   }
   catch (ParseError& e) {
      Serr << filename << "[" << e.pos << "]: " << e.msg << endl;
      goto compare;
   }
   try {
      Translator::translator.set_language("es");
      Stepper S(&Sin, &Saux);
      S.Step(program);
      while (!S.finished()) {
         Sout << S.span() << ": " << P.lexer().SubStr(S.span()) << endl;
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
   } 
   catch (Error* e) {
      Serr << "Error de ejecución: " << e->msg << endl;
   }

 compare:
   compare_result(filename, Sout.str(), Serr.str(), out, err);
   return 0;
}

struct Args {
   string filename;
   string cmd;
};

int help(string filename);

typedef int (*CmdFunc)(string);
map<string, CmdFunc> funcs = {
   {"vm",               test_vm},
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

void help() {
   cout << "usage: minicc [cmd] <filename>" << endl;
   cout << "Commands: " << endl;
   cout << "   --help" << endl;
   for (auto it = funcs.begin(); it != funcs.end(); it++) {
      cout << "   --" << it->first << endl;
   }
   cout << endl;
}

void Usage() {
   cerr << "usage: minicc [cmd] <filename>" << endl;
   exit(1);
}

bool IsOption(string s) {
   return s.substr(0, 2) == "--";
}

Args ParseArgs(int argc, char *argv[]) {
   Args A;
   switch (argc) {
   case 2:
      if (string("--help") == argv[1]) {
         help();
         exit(0);
      }
      if (IsOption(argv[1])) {
         Usage();
      }
      A.cmd = "eval";
      A.filename = argv[1];
      break;
   case 3:
      if (!IsOption(argv[1]) or IsOption(argv[2])) {
         Usage();
      }
      A.cmd = argv[1] + 2; // skip '--'
      A.filename = argv[2];
      break;
   default:
      Usage();
   }
   return A;
}

int main(int argc, char *argv[]) {
   Translator::translator.set_language("es");
   Args A = ParseArgs(argc, argv);
   auto it = funcs.find(A.cmd);
   if (it == funcs.end()) {
      cerr << "No se reconoce el comando '" << A.cmd << "'" << endl;
      return 1;
   } else {
      return (*it->second)(A.filename);
   }
}
