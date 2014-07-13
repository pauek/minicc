#include <cstdlib>
#include <sstream>
#include <assert.h>
#include "parser.hh"
using namespace std;

void Parser::error(string msg) {
   cerr << msg << endl;
   exit(1);   
}

void Parser::warning(string msg) {
   cerr << msg << endl;
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
         }
         ostringstream msg;
         msg << pos << ": Unexpected token '" << tok << "'";
         error(msg.str());
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
      _in.skip_to("\n");
      Pos macro_fin = _in.pos();
      _in.next();
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
      warning(_in.pos().str() + ": expected '\\n' after '#include' (skipped \"" + skipped + "\")");
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
