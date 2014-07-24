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

void Parser::error(AstNode *n, string msg) {
   Error *err = new Error(_in.pos(), msg);
   n->errors.push_back(err);
}

template<class Node>
typename Node::Error *Parser::error(string msg) {
   typename Node::Error *s = new typename Node::Error();
   s->code = _in.skip_to(";");
   error(s, msg);
   return s;
}

template<class X>
void Parser::_skip(X *n, std::string stopset) {
   n->comments.push_back(_in.skip(stopset));
}

void Parser::_skip(string stopset) {
   CommentSeq *cn = _in.skip(stopset);
   if (cn != 0) delete cn;
}

AstNode* Parser::parse() {
   Program *prog = new Program();
   _in.next();
   CommentSeq *c = _in.skip("\n\t ");
   if (c != 0) {
      prog->add(c);
   }
   while (!_in.end()) {
      Pos pos = _in.pos();
      Token tok = _in.peek_token();
      switch (tok.type) {
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
         prog->add(error<Stmt>("UNIMPLEMENTED"));
         _in.skip_to(";");
         break;
      }
      case Token::Empty: {
         ostringstream msg;
         msg << pos << ": Unexpected character '" << _in.curr() << "'";
         prog->add(error<Stmt>(msg.str()));
         _in.next_token();
         break;
      }
      default: {
         if (tok.group & Token::TypeSpec) {
            prog->add(parse_func_or_var());
         } else {
            ostringstream msg;
            msg << pos << ": No esperaba '" << tok.str << "'";
            prog->add(error<Stmt>(msg.str()));
            _in.skip_to("\n");
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
   vector<CommentSeq*> cmts;
   cmts.push_back(_in.skip("\t "));
   Pos macro_ini = _in.pos();
   if (!_in.expect("include")) {
      Token tok = _in.read_id();
      string macro_name = tok.str;
      _in.skip_to("\n");
      Pos macro_fin = _in.pos();
      _in.next();
      Macro *m = new Macro(_in.substr(macro_ini, macro_fin));
      error(m, ini.str() + ": ignoring macro '" + macro_name + "'");
      return m;
   }
   Include* inc = new Include();

   cmts.push_back(_in.skip("\t "));
   char open = _in.curr();
   if (open != '"' && open != '<') {
      error(inc, _in.pos().str() + ": expected '\"' or '<'");
      _in.skip_to("\n");
      return inc;
   }
   char close = (open == '"' ? '"' : '>');
   const bool is_global = (open == '<');
   string filename;
   _in.next();
   while (_in.curr() != close) {
      if (_in.curr() == '\n') {
         error(inc, _in.pos().str() + ": '#include' missing closing '" + close + "'");
         break;
      }
      filename += _in.curr();
      Pos p = _in.pos();
      _in.next();
      if (_in.end()) {
         error(inc, p.str() + ": '#include' missing closing '" + close + "'");
         break;
      }
   }
   CommentSeq *c3 = 0;
   if (_in.curr() == close) {
      _in.next();
      c3 = _in.skip("\t ");
   }
   cmts.push_back(c3);

   inc->filename = filename;
   inc->global = (close == '>');

   Pos fin = _in.pos();
   if (!_in.expect("\n")) {
      string skipped = _in.skip_to("\n");
      error(inc, fin.str() + ": expected '\\n' after '#include' (found \"" + skipped + "\")");
      _in.next();
   }
   inc->comments = cmts;
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
      error(u, u->ini.str() + ": expected 'namespace'");
      _in.skip_to("\n");
      return u;
   }
   _skip(u);
   Token tok = _in.read_id();
   u->namespc = tok.str;
   _skip(u);
   u->fin = _in.pos();
   if (!_in.expect(";")) {
      error(u, u->fin.str() + ": expected ';'");
   }
   _skip(u, "\t ");
   Pos p = _in.pos();
   string rest = _in.skip_to("\n");
   if (!is_space(rest)) {
      error(u, p.str() + ": extra text after 'using' declaration (\"" + rest + "\")");
   }
   _in.next();
   return u;
}

Type *Parser::parse_type() {
   Type *type = new Type();

   while (true) {
      Token tok = _in.peek_token();
      if (tok.group & Token::TypeQual) {
         _in.next_token();
         switch (tok.type) {
         case Token::Const:   type->qual |= Type::Const; break;
         case Token::Auto:    type->qual |= Type::Auto;  break;
         case Token::Mutable: type->qual |= Type::Mutable;  break;
         default: /* TODO: acabar! */ break;
         }
         _skip(type);
      } else if (tok.group & Token::BasicType or
                 (type->id == 0 and (tok.group & Token::Identifier))) {
         _in.next_token();
         type->id = new Identifier(tok.str);
         _skip(type->id);
      } else {
         break;
      }
   }
   return type;
}

AstNode *Parser::parse_func_or_var() {
   CommentSeq *c[2] = { 0, 0 };
   Pos ini = _in.pos();
   Type *type = parse_type();
   c[0] = _in.skip("\t ");
   Token tok = _in.read_id();
   string name = tok.str;
   c[1] = _in.skip("\t ");
   if (_in.curr() == '(') {
      FuncDecl *fn = new FuncDecl(name);
      fn->comments.assign(c, c+2);
      fn->return_type = type;
      fn->ini = ini;
      parse_function(fn);
      return fn;
   } else {
      delete type;
      return error<Stmt>("Global variables: UNIMPLEMENTED");
   }
   return NULL;
}

void Parser::parse_function(FuncDecl *fn) {
   CommentSeq *cn;

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
      p->name = tok.str;
      _skip(p);
      fn->params.push_back(p);

      if (_in.curr() == ')') {
         break;
      } else if (_in.curr() == ',') {
         _in.consume(',');
      } else {
         error(fn, string("Unexpected character '") + _in.curr() + "' in parameter list");
         _in.skip_to(")");
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
      error(block, "'{' expected");
      return block;
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
      error(block, "expected '}' but found EOF");
   }
   _skip(block);
   return block;
}

Stmt* Parser::parse_stmt() {
   Token tok1 = _in.peek_token();
   switch (tok1.type) {
   case Token::LParen:
      return parse_exprstmt();

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
      if (tok1.group == Token::Operator) {
         return parse_exprstmt();
      }
      return parse_decl_or_expr_stmt();
   }
}

Stmt *Parser::parse_decl_or_expr_stmt() {
   _in.save();
   Token tok1 = _in.next_token();
   _skip();
   Token tok2 = _in.next_token();
   _in.restore();
   if (tok2.group & Token::TypeSpec or tok2.group & Token::Identifier) {
      return parse_declstmt();
   } else {
      return parse_exprstmt();
   }
}

Stmt *Parser::parse_jumpstmt() {
   JumpStmt *stmt = new JumpStmt();
   stmt->ini = _in.pos();
   Token tok = _in.next_token();
   switch (tok.type) {
   case Token::Break:    stmt->type = JumpStmt::_break; break;
   case Token::Continue: stmt->type = JumpStmt::_continue; break;
   case Token::Goto:     stmt->type = JumpStmt::_goto; break;
   default:
      break;
   }
   _skip(stmt);
   if (stmt->type == JumpStmt::_goto) {
      Token tok = _in.read_id();
      stmt->label = tok.str;
      _skip(stmt);
   }
   if (!_in.expect(";")) {
      error(stmt, _in.pos().str() + ": Expected ';'");
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
      error(stmt, _in.pos().str() + ": Expected ';'");
      _in.skip_to(";\n"); // resync...
   }
   _skip(stmt);
   return stmt;
}

Expr *Parser::parse_primary_expr() {
   Expr *e;
   Token tok = _in.next_token();
   switch (tok.type) {
   case Token::LParen:
      e = parse_expr();
      e->paren = true;
      if (!_in.expect(")")) {
         error(e, _in.pos().str() + ": Expected ')'");
      }
      break;

   case Token::True:
   case Token::False: {
      Literal* lit = new Literal(Literal::Bool);
      lit->val.as_bool = (tok.type == Token::True);
      _skip(lit);
      e = lit;
      break;
   }
   case Token::IntLiteral: {
      Literal* lit = new Literal(Literal::Int);
      lit->val.as_int = atoi(_in.substr(tok).c_str());
      _skip(lit);
      e = lit;
      break;
   }
   case Token::CharLiteral: {
      Literal* lit = new Literal(Literal::Char);
      lit->val.as_string.s = new string(tok.str);
      _skip(lit);
      e = lit;
      break;
   }
   case Token::StringLiteral: {
      Literal* lit = new Literal(Literal::String);
      lit->val.as_string.s = new string(tok.str);
      _skip(lit);
      e = lit;
      break;
   }
   default:
      e = parse_identifier(tok);
      break;
   }
   return e;
}

Expr *Parser::parse_identifier(Token tok) {
   if (tok.type == Token::Empty) {
      return error<Expr>("Expression doesn't start with a token");
   }
   Identifier *e = new Identifier(_in.substr(tok));
   _skip(e);
   return e;
}

Expr *Parser::parse_postfix_expr(Expr *e = 0) {
   if (e == 0) {
      e = parse_primary_expr();
   }
 begin:
   Token tok = _in.peek_token();
   switch (tok.type) {
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
   switch (tok.type) {
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
      SignExpr *se = new SignExpr(tok.type == Token::Plus 
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
   CommentSeq *cn;

   Expr *left = parse_unary_expr();

   while (true) {
      _in.save();
      Token tok = _in.read_operator();
      BinaryExpr::Type type = BinaryExpr::tok2type(tok.type);
      if (tok.type == Token::Empty or type > max) {
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
      error(e, _in.pos().str() + ": Esperaba ')'");
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
      error(e, _in.pos().str() + ": Esperaba ']'");
   }
   _skip(e);
   return e;
}

Expr *Parser::parse_fieldexpr(Expr *x, Token tok) {
   FieldExpr *e = new FieldExpr();
   e->base = x;
   e->pointer = (tok.type == Token::Arrow);
   _in.consume(tok.type == Token::Arrow ? "->" : ".");
   _skip(e);
   Token id = _in.read_id();
   e->field = new Identifier(id.str);
   _skip(e->field);
   return e;
}

Expr *Parser::parse_increxpr(Expr *x, Token tok) {
   IncrExpr *e = new IncrExpr(Token::PlusPlus ? IncrExpr::Positive : IncrExpr::Negative);
   e->expr = x;
   _in.consume(tok.type == Token::PlusPlus ? "++" : "--");
   _skip(e);
   return e;
}

Stmt *Parser::parse_for() {
   IterStmt *stmt = new IterStmt();
   _in.consume("for");
   _skip(stmt);
   if (!_in.expect("(")) {
      error(stmt, _in.pos().str() + ": Expected '('");
      // TODO: resync?
   }
   _in.skip("\t\n "); // Comments here will disappear
   stmt->init = parse_decl_or_expr_stmt();
   stmt->cond = parse_expr();
   if (!_in.expect(";")) {
      error(stmt, _in.pos().str() + ": Expected ';'");
   }
   _in.skip("\t\n "); // Comments here will disappear
   stmt->post = parse_expr();
   if (!_in.expect(")")) {
      error(stmt, _in.pos().str() + ": Expected ')'");
   }
   _skip(stmt);
   stmt->substmt = parse_stmt();
   return stmt;
}

Stmt *Parser::parse_while() {
   IterStmt *stmt = new IterStmt();
   _in.consume("while");
   _skip(stmt);
   if (!_in.expect("(")) {
      error(stmt, _in.pos().str() + ": Expected '('");
   }
   _in.skip("\t\n "); // Comments here will disappear
   stmt->cond = parse_expr();
   if (!_in.expect(")")) {
      error(stmt, _in.pos().str() + ": Expected ')')");
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
      error(stmt, _in.pos().str() + ": Expected '('");
   }
   _in.skip("\t\n "); // Comments here will disappear
   stmt->cond = parse_expr();
   if (!_in.expect(")")) {
      error(stmt, _in.pos().str() + ": Expected ')')");
   }
   _skip(stmt);
   stmt->then = parse_stmt();
   
   string tok;
   if (_in.peek_token().type == Token::Else) {
      _in.consume("else");
      _skip(stmt);
      stmt->els = parse_stmt();
   }
   return stmt;
}

Stmt *Parser::parse_switch() {
   return error<Stmt>("UNIMPLEMENTED switch");
}

Stmt *Parser::parse_declstmt() {
   DeclStmt *stmt = new DeclStmt();
   stmt->type = parse_type();
   _skip(stmt);
   while (true) {
      Token id = _in.read_id();
      DeclStmt::Decl decl(id.str);
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
      error(stmt, _in.pos().str() + ": Esperaba un ';'");
   }
   _skip(stmt);
   return stmt;
}
