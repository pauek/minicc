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

bool is_bool_literal(string s) {
   return s == "true" or s == "false";
}

bool Parser::is_literal(string s) const {
   return all_digits(s) || is_bool_literal(s);
}

void Parser::error(string msg) {
   throw new ParseError(msg);
}

void Parser::warning(string msg) {
   (*_err) << msg << endl;
}

void Parser::_skip(AstNode *n) {
   n->comment_nodes.push_back(_in.skip("\n\t "));
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
   _skip(fn);
   if (_in.curr() == ';') {
      fn->block = 0;
      _in.next();
   } else {
      fn->block = parse_block();
   }
   _skip(fn);
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
   prm.comment_nodes[0] = _in.skip("\t ");
   if (_in.curr() == ')') {
      return false;
   }
   string typ = _in.next_token();
   if (typ == "") {
      return false;
   }
   prm.type = new Type(typ);
   prm.comment_nodes[1] = _in.skip("\t ");
   prm.name = _in.next_token();
   prm.comment_nodes[2] = _in.skip("\t ");
   return !_in.end();
}

Block *Parser::parse_block() {
   Block *block = new Block();
   block->ini = _in.pos();
   if (!_in.expect("{")) {
      error("'{' expected");
   }
   _skip(block);
   while (!_in.end()) {
      if (_in.curr() == '}') {
         _in.next();
         break;
      }
      Stmt *stmt = parse_stmt();
      block->stmts.push_back(stmt);
   }
   if (_in.end()) {
      error("expected '}' but found EOF");
   }
   _skip(block);
   return block;
}

Stmt* Parser::parse_stmt() {
   string tok = _in.peek_token();
   if (_in.curr() == '{') {
      return parse_block();
   } else if (is_type(tok)) {
      return parse_declstmt();
   }
   JumpStmt::Type type = JumpStmt::keyword2type(tok);
   if (type != JumpStmt::unknown) {
      return parse_jumpstmt(type);
   } else if (tok == "while") {
      return parse_while();
   } else if (tok == "for") {
      return parse_for();
   } else if (tok == "if") {
      return parse_ifstmt();
   } else if (tok == "switch") {
      return parse_switch();
   } else  {
      return parse_exprstmt();
   }
}

Stmt *Parser::parse_jumpstmt(JumpStmt::Type type) {
   string tok = _in.next_token();
   JumpStmt *stmt = new JumpStmt;
   stmt->type = type;
   _skip(stmt);
   if (type == JumpStmt::_goto) {
      stmt->label = _in.next_token();
      _skip(stmt);
   }
   if (!_in.expect(";")) {
      warning(_in.pos().str() + ": Expected ';'");
      _in.skip_to(";\n"); // resync...
   }
   _skip(stmt);
   return stmt;
}

Expr *Parser::parse_expr() {
   return parse_binaryexpr();
}

Stmt *Parser::parse_exprstmt() {
   ExprStmt *stmt = new ExprStmt();
   stmt->ini = _in.pos();
   stmt->expr = (_in.curr() == ';' ? 0 : parse_expr());
   stmt->fin = _in.pos();
   if (!_in.expect(";")) {
      warning(_in.pos().str() + ": Expected ';'");
      _in.skip_to(";\n"); // resync...
   }
   _skip(stmt);
   return stmt;
}

Expr *Parser::parse_binaryexpr(BinaryExpr::Type max) {
   CommentNode *cn;
   Expr *left;

   // Left
   if (_in.curr() == '(') {
      _in.next();
      left = parse_binaryexpr();
      left->paren = true;
      if (!_in.expect(")")) {
         error(_in.pos().str() + ": Expected ')'");
      }
   } else {
      string tok = _in.next_token();
      if (tok == "") {
         error("Expression doesn't start with a token");
         return 0;
      }
      if (is_literal(tok)) {
         Literal *x = new Literal();
         x->lit = tok;
         left = x;
      } else {
         BinaryExpr *e = new BinaryExpr();
         e->type = BinaryExpr::identifier;
         e->str = tok;
         left = e;
      }
   }
   _skip(left);

   // Function call?
   if (_in.curr() == '(') {
      error("UNIMPLEMENTED");
      return 0;
   }

   while (true) {
      _in.save();
      string op = _in.read_operator();
      BinaryExpr::Type type = BinaryExpr::op2type(op);
      if (op == "" or type > max) {
         _in.restore();
         break;
      }
      BinaryExpr *e = new BinaryExpr();
      e->set(op);
      _skip(e);
      BinaryExpr::Type submax = BinaryExpr::Type(type - 1);
      if (type == BinaryExpr::assignment) {
         submax = type;
      }
      Expr *right = parse_binaryexpr(submax);
      e->left = left;
      e->right = right;
      left = e;
   } 
   _skip(left);
   return left;
}

void Parser::_parse_while_or_if(Stmt *stmt, string which) {
   _in.consume(which);
   _skip(stmt);
   if (!_in.expect("(")) {
      error(_in.pos().str() + ": Expected '('");
   }
   _in.skip("\t\n "); // Comments here will disappear
   stmt->expr = parse_binaryexpr();
   if (!_in.expect(")")) {
      error(_in.pos().str() + ": Expected ')')");
   }
   _skip(stmt);
   stmt->sub_stmt[0] = parse_stmt();
}

Stmt *Parser::parse_for() {
   IterStmt *stmt = new IterStmt();
   _in.consume("for");
   _skip(stmt);
   if (!_in.expect("(")) {
      error(_in.pos().str() + ": Expected '('");
   }
   _in.skip("\t\n "); // Comments here will disappear
   stmt->init = parse_for_init_stmt();
   stmt->cond = parse_binaryexpr();
   if (!_in.expect(";")) {
      error(_in.pos().str() + ": Expected ';'");
   }
   _in.skip("\t\n "); // Comments here will disappear
   stmt->post = parse_binaryexpr();
   if (!_in.expect(")")) {
      error(_in.pos().str() + ": Expected ')'");
   }
   _skip(stmt);
   stmt->substmt = parse_stmt();
   return stmt;
}

Stmt *Parser::parse_for_init_stmt() {
   string tok = _in.peek_token();
   if (is_type(tok)) {
      return parse_declstmt();
   } else {
      return parse_exprstmt();
   }
}

Stmt *Parser::parse_while() {
   IterStmt *stmt = new IterStmt();
   _in.consume("while");
   _skip(stmt);
   if (!_in.expect("(")) {
      error(_in.pos().str() + ": Expected '('");
   }
   _in.skip("\t\n "); // Comments here will disappear
   stmt->cond = parse_binaryexpr();
   if (!_in.expect(")")) {
      error(_in.pos().str() + ": Expected ')')");
   }
   _skip(stmt);
   stmt->substmt = parse_stmt();
   return stmt;
}

Stmt *Parser::parse_ifstmt() {
   IfStmt *stmt = new IfStmt();

   _in.consume("if");
   _skip(stmt);
   if (!_in.expect("(")) {
      error(_in.pos().str() + ": Expected '('");
   }
   _in.skip("\t\n "); // Comments here will disappear
   stmt->cond = parse_expr();
   if (!_in.expect(")")) {
      error(_in.pos().str() + ": Expected ')')");
   }
   _skip(stmt);
   stmt->then = parse_stmt();
   
   string tok = _in.peek_token();
   if (tok == "else") {
      _in.consume("else");
      _skip(stmt);
      stmt->els = parse_stmt();
   }
   return stmt;
}

Stmt *Parser::parse_switch() {
   error("UNIMPLEMENTED");
   return 0;
}

Stmt *Parser::parse_declstmt() {
   error("UNIMPLEMENTED");
   return 0;
}
