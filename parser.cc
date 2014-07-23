#include <cstdlib>
#include <sstream>
#include <fstream>
#include <assert.h>
using namespace std;

#include "parser.hh"

Token::Type BasicTypes[] = { 
   Token::Int, Token::Char, Token::String, Token::Void, 
   Token::Bool, Token::Double, Token::Float 
};
set<Token::Type> Parser::_basic_types;

Parser::Parser(istream *i, std::ostream* err) : _in(i), _err(err) {
   for (Token::Type t : BasicTypes) {
      _basic_types.insert(t);
   }
}

bool Parser::is_builtin_type(Token::Type t) const {
   return _basic_types.find(t) != _basic_types.end();
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

template<class X>
void Parser::_skip(X *n, std::string stopset) {
   n->comment_nodes.push_back(_in.skip(stopset));
}

AstNode* Parser::parse() {
   Program *prog = new Program();
   _in.next();
   CommentNode *c = _in.skip("\n\t ");
   if (c != 0) {
      prog->add(c);
   }
   while (!_in.end()) {
      Pos pos = _in.pos();
      Token tok = _in.peek_token();
      switch (tok.t) {
      case Token::Sharp: {
         prog->add(parse_macro());
         break;
      }
      case Token::Using: {
         prog->add(parse_using_declaration());
         break;
      }
      case Token::Struct:
      case Token::Typedef:
      case Token::Class: {
         error("UNIMPLEMENTED");
      }
      case Token::Empty: {
         ostringstream msg;
         msg << pos << ": Unexpected character '" << _in.curr() << "'";
         error(msg.str());
      }
      default: {
         if (tok.k == Token::BasicType) {
            prog->add(parse_func_or_var());
         } else {
            ostringstream msg;
            msg << pos << ": Unexpected token '" << tok.t << "'";
            error(msg.str());
         }
      }
      }
      c = _in.skip("\n\t ");
      if (c != 0) {
         prog->add(c);
      }
   }
   return prog;
}

AstNode* Parser::parse_macro() {
   Pos ini = _in.pos();
   _in.consume('#');
   vector<CommentNode*> cmts;
   cmts.push_back(_in.skip("\t "));
   Pos macro_ini = _in.pos();
   if (!_in.expect("include")) {
      Token tok = _in.read_id();
      string macro_name = _in.substr(tok);
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
   Using *u = new Using();
   u->ini = _in.pos();
   _in.consume("using");
   _skip(u);
   if (!_in.expect("namespace")) {
      error(u->ini.str() + ": expected 'namespace'");
   }
   _skip(u);
   Token tok = _in.read_id();
   u->namespc = _in.substr(tok);
   _skip(u);
   u->fin = _in.pos();
   if (!_in.expect(";")) {
      warning(u->fin.str() + ": expected ';'");
   }
   _skip(u, "\t ");
   Pos p = _in.pos();
   string rest = _in.skip_to("\n");
   if (!is_space(rest)) {
      warning(p.str() + ": extra text after 'using' declaration (\"" + rest + "\")");
   }
   _in.next();
   return u;
}

Type *Parser::parse_type() {
   return new Type(_in.next_token_old());
}

AstNode *Parser::parse_func_or_var() {
   CommentNode *c[2] = { 0, 0 };
   Pos ini = _in.pos();
   Type *type = parse_type();
   c[0] = _in.skip("\t ");
   Token tok = _in.read_id();
   string name = _in.substr(tok);
   c[1] = _in.skip("\t ");
   if (_in.curr() == '(') {
      FuncDecl *fn = new FuncDecl(name);
      fn->comment_nodes.assign(c, c+2);
      fn->return_type = type;
      fn->ini = ini;
      parse_function(fn);
      return fn;
   } else {
      delete type;
      error("Global variables: UNIMPLEMENTED");
   }
   return NULL;
}

void Parser::parse_function(FuncDecl *fn) {
   CommentNode *cn;

   // parameter list
   _in.consume('(');
   while (true) {
      FuncDecl::Param *p = new FuncDecl::Param();
      _skip(p);
      if (_in.curr() == ')') {
         delete p;
         break;
      }
      p->type = parse_type();
      _skip(p);
      Token tok = _in.read_id();
      p->name = _in.substr(tok);
      _skip(p);
      fn->params.push_back(p);

      if (_in.curr() == ')') {
         break;
      } else if (_in.curr() == ',') {
         _in.consume(',');
      } else {
         error(string("Unexpected character '") + _in.curr() + "' in parameter list");
      }
   }
   _in.consume(')');
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
   Token t = _in.peek_token();
   switch (t.t) {
   case Token::LCurly:  
      return parse_block();

   case Token::Break: case Token::Continue: case Token::Goto:
      return parse_jumpstmt();

   case Token::While:
      return parse_while();

   case Token::For:
      return parse_for();

   case Token::If:
      return parse_ifstmt();

   case Token::Switch:
      return parse_switch();

   default:
      if (t.k == Token::BasicType) {
         return parse_declstmt();
      } else  {
         return parse_exprstmt();
      }
   }
}

Stmt *Parser::parse_jumpstmt() {
   JumpStmt *stmt = new JumpStmt();
   stmt->ini = _in.pos();
   Token tok = _in.next_token();
   switch (tok.t) {
   case Token::Break:    stmt->type = JumpStmt::_break; break;
   case Token::Continue: stmt->type = JumpStmt::_continue; break;
   case Token::Goto:     stmt->type = JumpStmt::_goto; break;
   default:
      break;
   }
   _skip(stmt);
   if (stmt->type == JumpStmt::_goto) {
      Token tok = _in.read_id();
      stmt->label = _in.substr(tok);
      _skip(stmt);
   }
   if (!_in.expect(";")) {
      warning(_in.pos().str() + ": Expected ';'");
      _in.skip_to(";\n"); // resync...
   }
   _skip(stmt);
   return stmt;
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


Expr *Parser::parse_primary_expr() {
   Expr *e;
   _in.save();
   Token tok = _in.next_token();
   switch (tok.t) {
   case Token::LParen:
      e = parse_expr();
      e->paren = true;
      if (!_in.expect(")")) {
         error(_in.pos().str() + ": Expected ')'");
      }
      break;

   case Token::True:
   case Token::False:
   case Token::IntLiteral:
   case Token::CharLiteral:
   case Token::StringLiteral:
      e = new Literal(_in.substr(tok));
      _skip(e);
      break;

   default:
      if (tok.t == Token::Empty) {
         error("Expression doesn't start with a token");
         return 0;
      }
      e = new Identifier(_in.substr(tok));
      _skip(e);
   }
   return e;
}

Expr *Parser::parse_postfix_expr(Expr *e = 0) {
   if (e == 0) {
      e = parse_primary_expr();
   }
 begin:
   Token tok = _in.peek_token();
   switch (tok.t) {
   case Token::LParen:
      e = parse_callexpr(e);
      goto begin;
      
   case Token::LBrack:
      e = parse_indexexpr(e);
      goto begin;
      
   case Token::Dot:
   case Token::Arrow:
      e = parse_fieldexpr(e, tok);
      goto begin;

   case Token::PlusPlus:
   case Token::MinusMinus:
      e = parse_increxpr(e, tok);
      goto begin;
      
   default:
      break;
   }
   return e;
}

Expr *Parser::parse_unary_expr() {
   Expr *e;
   Token tok = _in.peek_token();
   switch (tok.t) {
   case Token::Not: {
      NegExpr *ne = new NegExpr();
      _in.next();
      _skip(ne);
      ne->expr = parse_unary_expr();
      e = ne;
      break;
   }
   case Token::Plus:
   case Token::Minus: {
      SignExpr *se = new SignExpr(tok.t == Token::Plus 
                                  ? SignExpr::Positive
                                  : SignExpr::Negative);
      _in.next();
      _skip(se);
      se->expr = parse_unary_expr();
      e = se;
      break;
   }
   case Token::Amp: {
      AddrExpr *ae = new AddrExpr();
      _in.next();
      _skip(ae);
      ae->expr = parse_unary_expr();
      e = ae;
      break;
   }      
   default:
      e = parse_postfix_expr();
      break;
   }
   _skip(e);
   return e;
}

Expr *Parser::parse_expr(BinaryExpr::Type max) {
   CommentNode *cn;

   Expr *left = parse_unary_expr();

   while (true) {
      _in.save();
      Token tok = _in.read_operator();
      BinaryExpr::Type type = BinaryExpr::tok2type(tok.t);
      if (tok.t == Token::Empty or type > max) {
         _in.restore();
         break;
      }
      _in.discard();
      BinaryExpr *e = new BinaryExpr();
      e->op = _in.substr(tok);
      e->set(type);
      _skip(e);
      BinaryExpr::Type submax = 
         BinaryExpr::Type(Expr::right_associative(type) 
                          ? type 
                          : type - 1);
      Expr *right = parse_expr(submax);
      e->left = left;
      e->right = right;
      left = e;
      _skip(left);
   } 
   return left;
}

Expr *Parser::parse_callexpr(Expr *x) {
   CallExpr *e = new CallExpr();
   e->func = x;
   _in.consume('(');
   _skip(e);
   if (_in.curr() != ')') {
      e->args.push_back(parse_expr(Expr::assignment));
      while (_in.curr() == ',') {
         _in.next();
         _skip(e);
         e->args.push_back(parse_expr(Expr::assignment));
      }
   }
   if (!_in.expect(")")) {
      error(_in.pos().str() + ": Esperaba ')'");
   }
   _skip(e);
   return e;
}

Expr *Parser::parse_indexexpr(Expr *x) {
   IndexExpr *e = new IndexExpr();
   e->base = x;
   _in.consume('[');
   e->index = parse_expr();
   if (!_in.expect("]")) {
      error(_in.pos().str() + ": Esperaba ']'");
   }
   _skip(e);
   return e;
}

Expr *Parser::parse_fieldexpr(Expr *x, Token tok) {
   FieldExpr *e = new FieldExpr();
   e->base = x;
   e->pointer = (tok.t == Token::Arrow);
   _in.consume(tok.t == Token::Arrow ? "->" : ".");
   _skip(e);
   Token id = _in.read_id();
   e->field = new Identifier(_in.substr(id));
   _skip(e->field);
   return e;
}

Expr *Parser::parse_increxpr(Expr *x, Token tok) {
   IncrExpr *e = new IncrExpr(Token::PlusPlus ? IncrExpr::Positive : IncrExpr::Negative);
   e->expr = x;
   _in.consume(tok.t == Token::PlusPlus ? "++" : "--");
   _skip(e);
   return e;
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
   stmt->cond = parse_expr();
   if (!_in.expect(";")) {
      error(_in.pos().str() + ": Expected ';'");
   }
   _in.skip("\t\n "); // Comments here will disappear
   stmt->post = parse_expr();
   if (!_in.expect(")")) {
      error(_in.pos().str() + ": Expected ')'");
   }
   _skip(stmt);
   stmt->substmt = parse_stmt();
   return stmt;
}

Stmt *Parser::parse_for_init_stmt() {
   Token tok = _in.peek_token();
   if (tok.k == Token::BasicType) {
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
   stmt->cond = parse_expr();
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
   
   string tok;
   if (_in.peek_token().t == Token::Else) {
      _in.consume("else");
      _skip(stmt);
      stmt->els = parse_stmt();
   }
   return stmt;
}

Stmt *Parser::parse_switch() {
   error("UNIMPLEMENTED switch");
   return 0;
}

Stmt *Parser::parse_declstmt() {
   DeclStmt *stmt = new DeclStmt();
   stmt->type = parse_type();
   _skip(stmt);
   while (true) {
      Token id = _in.read_id();
      DeclStmt::Decl decl(_in.substr(id));
      _skip(stmt);
      if (_in.curr() == '=') {
         _in.next();
         _skip(stmt);
         decl.init = parse_expr(Expr::assignment);
      }
      stmt->decls.push_back(decl);
      if (_in.curr() != ',') {
         break;
      }
      _in.next();
      _skip(stmt);
   }
   if (!_in.expect(";")) {
      error(_in.pos().str() + ": Esperaba un ';'");
   }
   _skip(stmt);
   return stmt;
}
