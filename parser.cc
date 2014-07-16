#include <cstdlib>
#include <sstream>
#include <fstream>
#include <assert.h>
using namespace std;

#include "parser.hh"

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

bool all_digits(string s) {
   for (char c : s) {
      if (!(c >= '0' and c <= '9')) {
         return false;
      }
   }
   return true;
}

bool Parser::is_literal(string s) const {
   return all_digits(s);
}

void Parser::error(string msg) {
   throw new ParseError(msg);
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
         if (tok == "using") {
            res->add(parse_using_declaration());
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
   inc->comment_nodes = cmts;
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
   u->comment_nodes.assign(c, c+4);
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
      fn->comment_nodes.assign(c, c+2);
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
   CommentNode *cn;
   parse_parameter_list(fn->params);
   cn = _in.skip("\t\n ");
   fn->comment_nodes.push_back(cn);
   fn->block = new Stmt(Stmt::_block);
   fn->block->ini = _in.pos();
   parse_block(fn->block);
   cn = _in.skip("\t\n ");
   fn->comment_nodes.push_back(cn);
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

void Parser::parse_block(Stmt *block) {
   CommentNode *ncomm;
   if (!_in.expect("{")) {
      error("'{' expected");
   }
   ncomm = _in.skip("\t\n ");
   block->comment_nodes.push_back(ncomm);
   while (!_in.end()) {
      if (_in.curr() == '}') {
         _in.next();
         break;
      }
      Stmt *stmt = parse_stmt();
      block->sub_stmts.push_back(stmt);
   }
   if (_in.end()) {
      error("expected '}' but found EOF");
   }
}

Stmt* Parser::parse_stmt() {
   Stmt *stmt = new Stmt();
   stmt->ini = _in.pos();
   if (_in.curr() == ';') {
      parse_colon(stmt);
   } else {
      string tok = _in.peek_token();
      if (tok == "for") {
         parse_for(stmt);
      } else if (tok == "while") {
         parse_while(stmt);
      } else if (tok == "if") {
         parse_if(stmt);
      } else if (tok == "switch") {
         parse_switch(stmt);
      } else {
         parse_expr_stmt(stmt);
      }
   }
   return stmt;
}

void Parser::parse_colon(Stmt *stmt) {
   stmt->fin = _in.pos();
   if (!_in.expect(";")) {
      warning(_in.pos().str() + ": Expected ';'");
      _in.skip_to(";\n"); // resync...
   }
   CommentNode *ncomm = _in.skip("\t\n ");
   stmt->comment_nodes.push_back(ncomm);
}

void Parser::parse_expr_stmt(Stmt *stmt) {
   stmt->typ = Stmt::_expr;
   stmt->expr = new Expr();
   parse_expr(stmt->expr);
   parse_colon(stmt);
}

void Parser::parse_expr(Expr *expr) {
   CommentNode *cn;

   // Left
   string tok = _in.next_token();
   expr->left = new Expr(is_literal(tok) ? Expr::literal : Expr::identifier);
   expr->left->str = tok;
   cn = _in.skip("\t\n ");
   expr->comment_nodes.push_back(cn);

   // Op
   if (_in.curr() == '=') {
      expr->op = Expr::assign;
      _in.consume('=');
   } else {
      error(string("Unknown operator '") + _in.curr() + "'");
   }
   cn = _in.skip("\t\n ");
   expr->comment_nodes.push_back(cn);

   // Right
   tok = _in.next_token();
   expr->right = new Expr(is_literal(tok) ? Expr::literal : Expr::identifier);
   expr->right->str = tok;
   cn = _in.skip("\t\n ");
   expr->comment_nodes.push_back(cn);
}

void Parser::parse_for(Stmt *stmt) {
   stmt->typ = Stmt::_for;
   error("UNIMPLEMENTED");
}

void Parser::parse_while(Stmt *stmt) {
   stmt->typ = Stmt::_while;
   error("UNIMPLEMENTED");
}

void Parser::parse_if(Stmt *stmt) {
   stmt->typ = Stmt::_if;
   error("UNIMPLEMENTED");
}

void Parser::parse_switch(Stmt *stmt) {
   stmt->typ = Stmt::_switch;
   error("UNIMPLEMENTED");
}

