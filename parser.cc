#include <cstdlib>
#include <sstream>
#include <fstream>
#include <assert.h>
using namespace std;

#include "parser.hh"

Parser::Parser(istream *i, std::ostream* err) : _in(i), _err(err) {
   static const char *basic_types[] = {
      "int", "char", "string", "double", "float", "short", "long", "bool", "void",
      "vector",
   };
   for (int i = 0; i < sizeof(basic_types) / sizeof(char*); i++) {
      _types.insert(basic_types[i]);
   }
}

bool Parser::_type_exists(Type *t) {
   auto it = _types.find(t->str());
   return it != _types.end();
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
         prog->add(parse_struct());
         break;

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
      _in.save();
      Token tok = _in.next_token();
      if (tok.group & Token::TypeQual) {
         _in.discard();
         switch (tok.type) {
         case Token::Const:   type->qual |= Type::Const; break;
         case Token::Auto:    type->qual |= Type::Auto;  break;
         case Token::Mutable: type->qual |= Type::Mutable;  break;
         default: /* TODO: acabar! */ break;
         }
         _skip(type);
      } else if (tok.group & Token::BasicType) {
         _in.discard();
         Ident *id = new Ident(tok.str);
         _skip(id);
         if (!type->nested_ids.empty()) {
            error(type, "Los tipos básicos no deben estar anidados");
         }
         type->nested_ids.push_back(id);

      } else if (type->nested_ids.empty() and (tok.group & Token::Ident)) {
         _in.discard();
         parse_type_id(type, tok);
      } else {
         _in.restore();
         break;
      }
   }
   return type;
}

void Parser::parse_type_id(Type *type, Token tok) {
   while (true) {
      Ident *id = new Ident(tok.str);
      _skip(id);
      _in.save();
      Token tok2 = _in.next_token();
      if (tok2.type == Token::LT) { // template_id
         _in.discard();
         parse_type_list(id, id->subtypes);
         if (_in.curr() != '>') { // Do NOT call next_token here, since it will return ">>"
            error(id, "Esperaba un '>' aquí");
         } else {
            _in.next();
         }
         _skip(id);
      } else {
         _in.restore();
      }
      type->nested_ids.push_back(id);
      tok = _in.peek_token();
      if (tok.type != Token::ColonColon) {
         break;
      } else {
         _in.next_token();
         tok = _in.next_token();
         if (!(tok.group & Token::Ident)) {
            error(type, "Esperaba un identificador aquí");
         }
      }
   }
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
      Token tok = _in.peek_token();
      if (tok.type == Token::Amp) {
         p->ref = true;
         _in.next_token();
         _skip(p);
      }
      tok = _in.read_id();
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

   case Token::Return: {
      ExprStmt *stmt = parse_exprstmt(true);
      stmt->is_return = true;
      return stmt;
   }
   default:
      if (tok1.group == Token::Operator) {
         return parse_exprstmt();
      }
      return parse_decl_or_expr_stmt();
   }
}

Stmt *Parser::parse_decl_or_expr_stmt() {
   _in.save();
   DeclStmt *decl = parse_declstmt();
   if (decl->has_errors()) {
      _in.restore(); // backtracking
      return parse_exprstmt();
   } else {
      _in.discard();
      return decl;
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

ExprStmt *Parser::parse_exprstmt(bool is_return) {
   ExprStmt *stmt = new ExprStmt();
   stmt->ini = _in.pos();
   if (is_return) {
      Token tok = _in.next_token();
      assert(tok.type == Token::Return);
      _skip(stmt);
   }
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
   case Token::Dot:
   case Token::RealLiteral: {
      Literal* lit = new Literal(Literal::Double);
      istringstream S(tok.str);
      S >> lit->val.as_double;
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
      e = parse_ident(tok);
      break;
   }
   return e;
}

Expr *Parser::parse_ident(Token tok) {
   if (!(tok.group & Token::Ident)) {
      return error<Expr>(_in.pos().str() + ": Expected identifier");
   }
   Ident *id = new Ident(tok.str);
   _skip(id);
   tok = _in.peek_token();
   if (tok.type == Token::ColonColon) {
      _in.next_token();
      _skip(id);
      tok = _in.next_token();
      if (!(tok.group & Token::Ident)) {
         error(id, _in.pos().str() + ": Esperaba un identificador aquí");
      }
      id->shift(tok.str);
      _skip(id);
      tok = _in.peek_token();
   }
   _skip(id);
   return id;
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
   case Token::Star: {
      DerefExpr *ae = new DerefExpr();
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
      if (tok.type == Token::QMark) { // (... ? ... : ...)
         CondExpr *e = new CondExpr();
         e->cond = left;
         _skip(e);
         e->then = parse_expr(Expr::assignment); // Expr::comma?
         Token colon = _in.read_operator();
         if (colon.type != Token::Colon) {
            error(e, "Esperaba un ':' aquí");
         }
         _skip(e);
         e->els = parse_expr(Expr::assignment);
         left = e;
      } else {
         BinaryExpr *e = new BinaryExpr();
         e->op = _in.substr(tok);
         e->set(type);
         _skip(e);
         Expr::Type submax = 
            Expr::Type(Expr::right_associative(type) 
                       ? type 
                       : type - 1);
         Expr *right = parse_expr(submax);
         e->left = left;
         e->right = right;
         left = e;
      }
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
   e->field = new Ident(id.str);
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

void Parser::parse_expr_list(AstNode *n, vector<Expr*>& exprs) {
   exprs.push_back(parse_expr(Expr::assignment));
   while (_in.curr() == ',') {
      _in.next();
      _skip(n);
      exprs.push_back(parse_expr(Expr::assignment));
   }
}

void Parser::parse_type_list(AstNode *n, vector<Type*>& v) {
   v.push_back(parse_type());
   _skip(n);
   while (_in.curr() == ',') {
      _in.next();
      _skip(n);
      v.push_back(parse_type());
   }
}

Decl *Parser::_parse_vardecl(string name, bool pointer) {
   VarDecl *decl = new VarDecl();
   _skip(decl); // after the name
   decl->name = name;
   decl->pointer = pointer;
   if (_in.curr() == '=') {
      _in.next();
      _skip(decl);
      decl->init = parse_expr(Expr::assignment);
   }
   return decl;
}

Decl *Parser::_parse_arraydecl(string name, bool pointer) {
   ArrayDecl *decl = new ArrayDecl();
   _skip(decl); // after the name
   decl->name = name;
   decl->pointer = pointer;
   _in.consume("[");
   _skip(decl);
   decl->size = parse_expr(Expr::conditional);
   if (!_in.expect("]")) {
      error(decl, _in.pos().str() + ": Esperaba un ']' aquí");
   }
   _skip(decl);
   if (_in.curr() == '=') {
      _in.next();
      _skip(decl);
      if (!_in.expect("{")) {
         error(decl, _in.pos().str() + ": Esperaba un '{' aquí");
      }
      _in.next();
      _skip(decl);
      parse_expr_list(decl, decl->init);
      if (!_in.expect("}")) {
         error(decl, _in.pos().str() + ": Esperaba un '}' aquí");
         _in.skip_to("},;\n");
      }
   }
   return decl;
}

Decl *Parser::_parse_objdecl(string name) {
   _in.consume("(");
   ObjDecl *decl = new ObjDecl();
   _skip(decl); // after the name
   decl->name = name;
   parse_expr_list(decl, decl->args);
   if (!_in.expect(")")) {
      error(decl, _in.pos().str() + ": Esperaba un ')' aquí");
      _in.skip_to("),;\n");
   }
   return decl;
}

DeclStmt *Parser::parse_declstmt() {
   DeclStmt *stmt = new DeclStmt();
   Type *type = parse_type();
   stmt->type = type;
   _skip(stmt);
   while (true) {
      Token id = _in.next_token();
      string name = id.str;
      bool pointer = false;
      if (id.type == Token::Star) {
         pointer = true;
         _skip(stmt);
         id = _in.next_token();
         name = id.str;
      }
      if (id.group != Token::Ident) {
         error(stmt, "Esperaba un identificador aquí");
      }
      Decl *decl;
      if (_in.curr() == '(') {
         decl = _parse_objdecl(name);
      } else if (_in.curr() == '[') {
         decl = _parse_arraydecl(name, pointer);
      } else {
         decl = _parse_vardecl(name, pointer);
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

AstNode *Parser::parse_struct() {
   Token tok = _in.next_token();
   assert(tok.type == Token::Struct);

   StructDecl *decl = new StructDecl();
   _skip(decl);

   decl->id = new Ident(_in.read_id().str);
   _skip(decl);
   
   tok = _in.next_token();
   if (tok.type != Token::LCurly) {
      error(decl, "Esperaba un '{' aquí");
      _in.skip_to("};");
      return decl;
   }
   _skip(decl);
   
   tok = _in.peek_token();
   while (!_in.end() and tok.type != Token::RCurly) {
      decl->decls.push_back(parse_declstmt());
      _skip(decl);
      tok = _in.peek_token();      
   }
   if (tok.type != Token::RCurly) {
      error(decl, _in.pos().str() + ": Esperaba un '}' aquí");
   }
   _in.next_token();
   tok = _in.next_token();
   if (tok.type != Token::SemiColon) {
      error(decl, _in.pos().str() + ": Esperaba un ';' aquí");
   }
   return decl;   
}
