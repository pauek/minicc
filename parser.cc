#include <cstdlib>
#include <sstream>
#include <fstream>
#include <assert.h>
using namespace std;

#include "parser.hh"
#include "prettyprint.hh"

set<string> Parser::_types;

Parser::Parser(istream *i, std::ostream* err) : _in(i), _err(err) {
   string T[] = { "int", "char", "string", "void", "bool", "double", "float" };
   for (string t : T) {
      _types.insert(t);
   }
}

bool Parser::is_type(string t) const {
   return _types.find(t) != _types.end();
}

void Parser::error(string msg) {
   (*_err) << msg << endl;
   exit(1);   
}

void Parser::warning(string msg) {
   (*_err) << msg << endl;
}

AstNode* Parser::parse() {
   Program *res = new Program();
   _in.next();
   CommentNode *c = _in.skip("\n\t ");
   if (c != 0) {
      res->add(c);
   }
   while (!_in.end()) {
      if (_in.curr() == '#') {
         res->add(parse_macro());
      } else {
         Pos pos = _in.pos();
         string tok = _in.peek_token();
         // 1) using namespace std;
         // 2) definición de tipo (typedef, struct, class)
         // 3) funcion.
         if (tok == "using") {
            res->add(parse_using_declaration());
            continue;
         } else if (tok == "struct" || tok == "typedef" || tok == "class") {
            error("'" + tok + "' is not supported yet");
         } else if (is_type(tok)) {
            res->add(parse_func_or_var(tok));
         } else if (tok == "") {
            ostringstream msg;
            msg << pos << ": Unexpected character '" << _in.curr() << "'";
            error(msg.str());
         } else {
            ostringstream msg;
            msg << pos << ": Unexpected token '" << tok << "'";
            error(msg.str());
         }
      }
      c = _in.skip("\n\t ");
      if (c != 0) {
         res->add(c);
      }
   }
   return res;
}

AstNode* Parser::parse_macro() {
   Pos ini = _in.pos();
   _in.consume('#');
   vector<CommentNode*> cmts;
   cmts.push_back(_in.skip("\t "));
   Pos macro_ini = _in.pos();
   if (!_in.expect("include")) {
      string macro_name = _in.next_token();
      _in.skip_to("\n");
      Pos macro_fin = _in.pos();
      _in.next();
      warning(ini.str() + ": ignoring macro '" + macro_name + "'");
      return new Macro(_in.substr(macro_ini, macro_fin));
   }
   cmts.push_back(_in.skip("\t "));
   char open = _in.curr();
   if (open != '"' && open != '<') {
      error(_in.pos().str() + ": expected '\"' or '<'");
   }
   char close = (open == '"' ? '"' : '>');
   const bool is_global = (open == '<');
   string filename;
   _in.next();
   while (_in.curr() != close) {
      if (_in.curr() == '\n') {
         warning(_in.pos().str() + ": '#include' missing closing '" + close + "'");
         break;
      }
      if (_in.curr() == '>' || _in.curr() == '"') {
         warning(_in.pos().str() + ": '#include' inconsistent delimiter '" + _in.curr() + "'");
         _in.next();
         break;
      }
      filename += _in.curr();
      Pos p = _in.pos();
      _in.next();
      if (_in.end()) {
         warning(p.str() + ": '#include' missing closing '" + close + "'");
         break;
      }
   }
   CommentNode *c3 = 0;
   if (_in.curr() == close) {
      _in.next();
      c3 = _in.skip("\t ");
   }
   cmts.push_back(c3);
   Pos fin = _in.pos();
   if (!_in.expect("\n")) {
      string skipped = _in.skip_to("\n");
      warning(fin.str() + ": expected '\\n' after '#include' (found \"" + skipped + "\")");
      _in.next();
   }
   AstNode* inc = new Include(filename, is_global);
   inc->comments = cmts;
   inc->ini = ini;
   inc->fin = fin;
   return inc;
}

AstNode* Parser::parse_using_declaration() {
   CommentNode *c[4] = { 0, 0, 0, 0 };
   Pos ini = _in.pos();
   _in.consume("using");
   c[0] = _in.skip("\t ");
   if (!_in.expect("namespace")) {
      error(ini.str() + ": expected 'namespace'");
   }
   c[1] = _in.skip("\t ");
   string namespc = _in.next_token();
   c[2] = _in.skip("\t ");
   Pos fin = _in.pos();
   if (!_in.expect(";")) {
      warning(fin.str() + ": expected ';'");
   }
   c[3] = _in.skip("\t ");
   Pos p = _in.pos();
   string rest = _in.skip_to("\n");
   if (!is_space(rest)) {
      warning(p.str() + ": extra text after 'using' declaration (\"" + rest + "\")");
   }
   _in.next();
   Using *u = new Using(namespc);
   u->comments.assign(c, c+4);
   u->ini = ini;
   u->fin = fin;
   return u;
}

AstNode *Parser::parse_func_or_var(string typ) {
   CommentNode *c[2] = { 0, 0 };
   Pos ini = _in.pos();
   _in.consume(typ);
   c[0] = _in.skip("\t ");
   string name = _in.next_token();
   c[1] = _in.skip("\t ");
   if (_in.curr() == '(') {
      FuncDecl *fn = new FuncDecl(name);
      fn->comments.assign(c, c+2);
      fn->return_type = new Type(typ);
      fn->ini = ini;
      parse_function(fn);
      return fn;
   } else {
      error("Variable declaration is unimplemented");
   }
   return NULL;
}

void Parser::parse_function(FuncDecl *fn) {
   parse_parameter_list(fn->params);
   CommentNode *cn = _in.skip("\t\n ");
   fn->comments.push_back(cn);
   fn->block = new Block();
   parse_block(fn->block);
   fn->fin = _in.pos();
}

void Parser::parse_parameter_list(vector<FuncDecl::Param>& params) {
   _in.consume('(');
   FuncDecl::Param p;
   while (parse_param(p)) {
      params.push_back(p);
      if (_in.curr() == ')') {
         break;
      } else if (_in.curr() == ',') {
         _in.consume(',');
      } else {
         error(string("Unexpected character '") + _in.curr() + "' in parameter list");
      }
   }
   _in.consume(')');
}

bool Parser::parse_param(FuncDecl::Param& prm) {
   prm.c[0] = _in.skip("\t ");
   if (_in.curr() == ')') {
      return false;
   }
   string typ = _in.next_token();
   if (typ == "") {
      return false;
   }
   prm.type = new Type(typ);
   prm.c[1] = _in.skip("\t ");
   prm.name = _in.next_token();
   prm.c[2] = _in.skip("\t ");
   return !_in.end();
}

void Parser::parse_block(Block *b) {
   _in.skip("\t\n ");
   if (!_in.expect("{")) {
      error("'{' expected");
   }
   CommentNode *cn = _in.skip("\t\n ");
   if (!_in.expect("}")) {
      error("'}' expected");
   }
   b->comments.push_back(cn);
}

/// Test the parser //////////////////////////////////////////////////

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
   AstNode *program = P.parse();
   PrettyPrinter pr(&Sout);
   program->visit(&pr);

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
      cerr << "target  \"\"\"" << visible_spaces(Serr.str()) << "\"\"\"" << endl;
      cerr << "current \"\"\"" << visible_spaces(err) << "\"\"\"" << endl;
      cerr << endl;
      res = 'x';
   }
   cout << res << flush;
}
