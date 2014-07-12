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
   while (!_in.end()) {
      switch (_in.curr()) {
      case '#':
         res->add(parse_macro());
         break;
      default:
         ostringstream msg;
         msg << "Unexpected character " << int(_in.curr()) << endl;
         error(msg.str());
      }
   }
   return res;
}

AstNode* Parser::parse_macro() {
   assert(_in.curr() == '#');
   _in.next();
   _in.skip_space();
   Pos ini = _in.pos();
   if (!_in.expect("include")) {
      _in.skip_to('\n');
      Pos fin = _in.pos();
      _in.next();
      return new Macro(_in.substr(ini, fin));
   }
   _in.skip_space();
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
   if (_in.curr() == close) {
      _in.next();
      _in.skip_space();
   }
   if (!_in.expect("\n")) {
      Pos begin = _in.pos();
      _in.skip_to('\n');
      Pos end = _in.pos();
      string skipped = _in.substr(begin, end);
      warning(_in.pos().str() + ": expected '\\n' after '#include' (skipped \"" + skipped + "\")");
      _in.next();
   }
   return new Include(filename, is_global);
}

void NodeList::visit(AstVisitor* v) {
   v->visit_nodelist(this);
   for (int i = 0; i < _children.size(); i++) {
      _children[i]->visit(v);
   }
}
