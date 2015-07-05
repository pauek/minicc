#include <cstdlib>
#include <sstream>
#include <fstream>
#include <assert.h>
using namespace std;

#include "parser.hh"
#include "translator.hh"

Parser::Parser(istream *i, std::ostream* err) : _in(i), _err(err) {
   static const char *basic_types[] = {
      "int", "char", "string", "double", "float", "short", "long", "bool", "void",
      "vector", "list", "map", "set", "pair"
   };
   for (int i = 0; i < sizeof(basic_types) / sizeof(char*); i++) {
      _types.insert(basic_types[i]);
   }
}

bool Parser::_is_type(string s) {
   return _types.find(s) != _types.end();
}

void Parser::error(AstNode *n, string msg) {
   Error *err = new Error(n->ini, n->fin, msg);
   n->errors.push_back(err);
}

void Parser::error(AstNode *n, Pos ini, Pos fin, string msg) {
   Error *err = new Error(ini, fin, msg);
   n->errors.push_back(err);
}

template<class Node>
typename Node::Error *Parser::error(string msg) {
   typename Node::Error *s = new typename Node::Error();
   s->code = _in.skip_to(";");
   error(s, msg);
   return s;
}


AstNode* Parser::parse() {
   Program *prog = new Program();
   if (!_in.next()) {
      error(prog, _T("Error when reading input"));
      return prog;
   }
   _skip(prog);
   while (!_in.end()) {
      Pos pos = _in.pos();
      Token tok = _in.peek_token();
      switch (tok.kind) {
      case Token::Sharp: {
         prog->add(parse_macro(prog));
         break;
      }
      case Token::Using: {
         prog->add(parse_using_declaration(prog));
         break;
      }
      case Token::Struct: {
         StructDecl *decl = parse_struct(prog);
         _types.insert(decl->id->name);
         prog->add(decl);
         break;
      }
      case Token::Typedef: {
         TypedefDecl *typdef = parse_typedef(prog);
         _types.insert(typdef->decl->name);
         prog->add(typdef);
         break;
      }
      case Token::Enum: {
         EnumDecl *enumdecl = parse_enum(prog);
         _types.insert(enumdecl->name);
         prog->add(enumdecl);
         break;
      }
      case Token::Class: {
         prog->add(error<Stmt>(_T("UNIMPLEMENTED")));
         _in.skip_to(";");
         break;
      }
      case Token::Empty: {
         ostringstream msg;
         msg << pos << ": " << _T("Unexpected character '%c'", _in.curr());
         prog->add(error<Stmt>(msg.str()));
         _in.next_token();
         break;
      }
      default:
         if (tok.group & Token::Ident or 
             tok.group & Token::TypeSpec) {
            prog->add(parse_func_or_var(prog));
            break;
         }
         error(prog, _T("Unexpected '%s' here.", tok.str.c_str()));
         _in.next_token();
         break;
      }
      _skip(prog);
   }
   return prog;
}

AstNode* Parser::parse_macro(AstNode *parent) {
   Pos ini = _in.pos();
   _in.consume('#');
   _in.skip("\t "); // comments between '#' and the macro name are gobbled up...
   Pos macro_ini = _in.pos();
   if (!_in.expect("include")) {
      Token tok = _in.read_id();
      string macro_name = tok.str;
      _in.skip_to("\n");
      Pos macro_fin = _in.pos();
      _in.next();
      Macro *m = new Macro(_in.substr(macro_ini, macro_fin));
      m->ini = ini;
      m->fin = macro_fin;
      error(m, macro_ini, macro_fin, _T("Macro '#%s' unknown.", macro_name.c_str()));
      return m;
   }
   Include* inc = new Include();
   inc->ini = ini;
   _skip(inc);
   char open = _in.curr();
   if (open != '"' && open != '<') {
      error(inc, _in.pos().str() + ": " + _T("Expected '\"' or '<' here."));
      _in.skip_to("\n");
      return inc;
   }
   char close = (open == '"' ? '"' : '>');
   const bool is_global = (open == '<');
   string filename;
   _in.next();
   while (_in.curr() != close) {
      if (_in.curr() == '\n') {
         Pos fin = _in.pos();
         inc->fin = fin;
         error(inc, fin, fin.next(), 
               _T("'#include' missing closing '%c'.", close));
         break;
      }
      filename += _in.curr();
      Pos p = _in.pos();
      _in.next();
      if (_in.end()) {
         error(inc, p, p.next(), _T("'#include' missing closing '%c'", close));
         break;
      }
   }
   if (_in.curr() == close) {
      _in.next();
   }
   inc->filename = filename;
   inc->global = (close == '>');
   inc->ini = ini;
   inc->fin = _in.pos();
   return inc;
}

AstNode* Parser::parse_using_declaration(AstNode *parent) {
   Using *u = new Using();
   u->ini = _in.pos();
   _in.consume("using");
   _skip(u);
   if (!_in.expect("namespace")) {
      error(u, u->ini.str() + ": " + _T("Expected '%s' here.", "namespace"));
      _in.skip_to("\n");
      return u;
   }
   _skip(u);
   Token tok = _in.read_id();
   u->namespc = tok.str;
   _skip(u);
   u->fin = _in.pos();
   if (!_in.expect(";")) {
      error(u, u->fin.str() + ": " + _T("Expected '%s' here.", ";"));
   }
   return u;
}

FullIdent *Parser::parse_ident(AstNode *parent, Token tok, Pos ini) {
   FullIdent *id = new FullIdent(tok.str);
   Pos fin = _in.pos();
   while (true) {
      tok = _in.peek_token();
      if (_is_type(id->name) and tok.kind == Token::LT) { // template_id
         _skip(id);
         _in.consume("<");
         _skip(id);
         parse_type_seq(id, id->subtypes);
         _skip(id);
         if (_in.curr() != '>') { // Do NOT call next_token here, since it will return ">>"
            error(id, _T("Expected '%s' here.", ">"));
         } else {
            _in.next();
         }
         fin = _in.pos();
      }
      tok = _in.peek_token();
      if (tok.kind != Token::ColonColon) {
         break;
      }
      _skip(id);
      _in.consume("::");
      _skip(id);
      tok = _in.next_token();
      if (!(tok.group & Token::Ident)) {
         error(id, _T("Expected an identifier here"));
      }
      id->shift(tok.str);
      fin = _in.pos();
   }
   id->ini = ini;
   id->fin = fin;
   return id;
}

bool Parser::_parse_type_process_token(TypeSpec *type, Token tok, Pos p) {
   if (tok.group & Token::BasicType) {
      if (type->id != 0) {
         error(type, _T("Basic types are not templates"));
      }
      type->id = new FullIdent(tok.str);
      return true;
   } 
   if (tok.group & Token::TypeQual) {
      switch (tok.kind) {
      case Token::Const:    type->qual.push_back(TypeSpec::Const);    break;
      case Token::Auto:     type->qual.push_back(TypeSpec::Auto);     break;
      case Token::Mutable:  type->qual.push_back(TypeSpec::Mutable);  break;
      case Token::Register: type->qual.push_back(TypeSpec::Register); break;
      case Token::Volatile: type->qual.push_back(TypeSpec::Volatile); break;
      case Token::Extern:   type->qual.push_back(TypeSpec::Extern);   break;
      default: /* TODO: acabar! */ break;
      }
      return true;
   } 
   if (type->id == 0 and (tok.group & Token::Ident)) {
      type->id = parse_ident(type, tok, p);
      return true;
   } 
   if (tok.kind == Token::Amp) {
      type->reference = true;
      return true;
   }
   return false;
}

TypeSpec *Parser::parse_typespec(AstNode *parent) {
   TypeSpec *type = new TypeSpec();
   type->parent = parent;

   Pos p = _in.pos();
   _in.save();
   Token tok = _in.next_token();
   while (_parse_type_process_token(type, tok, p)) {
      _in.discard();
      _in.save();
      _skip(type);
      p = _in.pos(), tok = _in.next_token();
   }
   _in.restore();
   return type;
}


AstNode *Parser::parse_func_or_var(AstNode *parent) {
   CommentSeq *c[2] = { 0, 0 };
   Pos ini = _in.pos();
   _in.save();
   TypeSpec *typespec = parse_typespec(0);
   c[0] = _in.skip("\n\t ");
   Pos id_ini = _in.pos();
   Token tok = _in.read_id();
   FullIdent *id = parse_ident(0, tok, id_ini);
   c[1] = _in.skip("\n\t ");
   if (_in.curr() == '(') {
      _in.discard();
      FuncDecl *fn = new FuncDecl(id);
      fn->parent = parent;
      id->parent = fn;
      fn->comments.assign(c, c+2);
      fn->return_typespec = typespec;
      typespec->parent = fn;
      fn->ini = ini;
      parse_function(fn);
      return fn;
   } else {
      delete typespec;
      _in.restore();
      return parse_declstmt(parent);
   }
   return NULL;
}

void Parser::parse_function(FuncDecl *fn) {
   CommentSeq *cn;

   // parameter list
   _in.consume('(');
   while (true) {
      _skip(fn);
      if (_in.curr() == ')') {
         break;
      }
      FuncDecl::Param *p = new FuncDecl::Param();
      p->ini = _in.pos();
      p->typespec = parse_typespec(fn);
      _skip(fn);
      Token tok = _in.read_id();
      p->name = tok.str;
      _skip(fn);
      fn->params.push_back(p);
      p->fin = _in.pos();

      if (_in.curr() == ')') {
         break;
      } else if (_in.curr() == ',') {
         _in.consume(',');
      } else {
         error(fn, _T("Unexpected character '%c' in parameter list", _in.curr()));
         _in.skip_to(")");
      }
   }
   _in.consume(')');
   _skip(fn);
   if (_in.curr() == ';') {
      fn->block = 0;
      _in.next();
   } else {
      fn->block = parse_block(fn);
   }
   fn->fin = _in.pos();
}

Block *Parser::parse_block(AstNode *parent) {
   Block *block = new Block();
   block->parent = parent;
   block->ini = _in.pos();
   if (!_in.expect("{")) {
      error(block, _T("I expected a '%s' here.", "{"));
      return block;
   }
   _skip(block);
   bool closing_curly = false;
   while (!_in.end()) {
      if (_in.curr() == '}') {
         closing_curly = true;
         _in.next();
         break;
      }
      Stmt *stmt = parse_stmt(block);
      block->stmts.push_back(stmt);
      _skip(block);
   }
   if (!closing_curly) {
      error(block, _T("Expected '}' but end of text found"));
   }
   block->fin = _in.pos();
   return block;
}

Stmt* Parser::parse_stmt(AstNode *parent) {
   Token tok1 = _in.peek_token();
   switch (tok1.kind) {
   case Token::LParen:
      return parse_exprstmt(parent);

   case Token::LCurly:  
      return parse_block(parent);

   case Token::Break: case Token::Continue: case Token::Goto:
      return parse_jumpstmt(parent);

   case Token::While:
      return parse_while(parent);

   case Token::For:
      return parse_for(parent);

   case Token::If:
      return parse_ifstmt(parent);

   case Token::Switch:
      return parse_switch(parent);

   case Token::Return: {
      ExprStmt *stmt = parse_exprstmt(parent, true);
      stmt->is_return = true;
      return stmt;
   }
   default:
      if (tok1.group == Token::Operator) {
         return parse_exprstmt(parent);
      }
      return parse_decl_or_expr_stmt(parent);
   }
}

Stmt *Parser::parse_decl_or_expr_stmt(AstNode *parent) {
   _in.save();
   DeclStmt *decl = parse_declstmt(parent);
   if (decl->has_errors()) {
      _in.restore(); // backtracking
      delete decl;
      return parse_exprstmt(parent);
   } else {
      _in.discard();
      return decl;
   }
}

Stmt *Parser::parse_jumpstmt(AstNode *parent) {
   JumpStmt *stmt = new JumpStmt();
   stmt->parent = parent;
   stmt->ini = _in.pos();
   Token tok = _in.next_token();
   switch (tok.kind) {
   case Token::Break:    stmt->kind = JumpStmt::Break; break;
   case Token::Continue: stmt->kind = JumpStmt::Continue; break;
   case Token::Goto:     stmt->kind = JumpStmt::Goto; break;
   default:
      break;
   }
   _skip(stmt);
   if (stmt->kind == JumpStmt::Goto) {
      Token tok = _in.read_id();
      stmt->label = tok.str;
      _skip(stmt);
   }
   if (!_in.expect(";")) {
      error(stmt, _in.pos().str() + ": " 
            + _T("Esperaba un ';' después de '%s'.", tok.str.c_str()));
      _in.skip_to(";\n"); // resync...
   }
   return stmt;
}

ExprStmt *Parser::parse_exprstmt(AstNode *parent, bool is_return) {
   ExprStmt *stmt = new ExprStmt();
   stmt->parent = parent;
   stmt->ini = _in.pos();
   if (is_return) {
      Token tok = _in.next_token();
      assert(tok.kind == Token::Return);
      _skip(stmt);
   }
   Pos eini = _in.pos();
   stmt->expr = (_in.curr() == ';' ? 0 : parse_expr(stmt));
   Pos efin = _in.pos();
   if (stmt->expr) {
      stmt->expr->ini = eini;
      stmt->expr->fin = efin;
   }
   _skip(stmt);
   stmt->fin = _in.pos();
   if (!_in.expect(";")) {
      error(stmt, _in.pos().str() + ": " + _T("Expected ';' after expression"));
      _in.skip_to(";\n"); // resync...
   }
   return stmt;
}

Expr *Parser::parse_primary_expr(AstNode *parent) {
   Expr *e;
   Pos ini = _in.pos();
   Token tok = _in.next_token();
   switch (tok.kind) {
   case Token::LParen: {
      CommentSeq *cn = _in.skip("\n\t ");
      e = parse_expr(parent);
      e->paren = true;
      e->comments.insert(e->comments.begin(), cn);
      _skip(e);
      if (!_in.expect(")")) {
         error(e, _in.pos().str() + ": Expected ')'");
      }
      e->ini = ini;
      e->fin = _in.pos();
      break;
   }
   case Token::True:
   case Token::False: {
      Literal* lit = new Literal(Literal::Bool);
      lit->parent = parent;
      lit->val.as_bool = (tok.kind == Token::True);
      lit->ini = ini;
      lit->fin = _in.pos();
      _skip(lit);
      e = lit;
      break;
   }
   case Token::IntLiteral: {
      Literal* lit = new Literal(Literal::Int);
      lit->parent = parent;
      lit->val.as_int = atoi(_in.substr(tok).c_str());
      lit->ini = ini;
      lit->fin = _in.pos();
      _skip(lit);
      e = lit;
      break;
   }
   case Token::CharLiteral: {
      Literal* lit = new Literal(Literal::Char);
      lit->parent = parent;
      lit->val.as_char = tok.str[0];
      lit->ini = ini;
      lit->fin = _in.pos();
      _skip(lit);
      e = lit;
      break;
   }
   case Token::Dot:
   case Token::RealLiteral: {
      Literal* lit = new Literal(Literal::Double);
      istringstream S(tok.str);
      S >> lit->val.as_double;
      lit->parent = parent;
      lit->ini = ini;
      lit->fin = _in.pos();
      _skip(lit);
      e = lit;
      break;
   }
   case Token::StringLiteral: {
      Literal* lit = new Literal(Literal::String);
      lit->parent = parent;
      lit->val.as_string.s = new string(tok.str);
      lit->ini = ini;
      lit->fin = _in.pos();
      _skip(lit);
      e = lit;
      break;
   }
   default:
      e = parse_ident(parent, tok, ini);
      break;
   }
   return e;
}


Expr *Parser::parse_postfix_expr(AstNode *parent, Expr *e = 0) {
   if (e == 0) {
      e = parse_primary_expr(parent);
   }
 begin:
   Token tok = _in.peek_token();
   switch (tok.kind) {
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

Expr *Parser::parse_unary_expr(AstNode *parent) {
   Expr *e;
   Pos ini = _in.pos();
   Token tok = _in.peek_token();
   switch (tok.kind) {
   case Token::Not: {
      NegExpr *ne = new NegExpr();
      _in.next();
      _skip(ne);
      ne->expr = parse_unary_expr(ne);
      e = ne;
      break;
   }
   case Token::Plus:
   case Token::Minus: {
      SignExpr *se = new SignExpr(tok.kind == Token::Plus
                                  ? SignExpr::Positive
                                  : SignExpr::Negative);
      _in.next();
      _skip(se);
      se->expr = parse_unary_expr(se);
      se->fin = se->expr->fin;
      e = se;
      break;
   }
   case Token::Amp: {
      AddrExpr *ae = new AddrExpr();
      _in.next();
      _skip(ae);
      ae->expr = parse_unary_expr(ae);
      ae->fin = ae->expr->fin;
      e = ae;
      break;
   }      
   case Token::Star: {
      DerefExpr *de = new DerefExpr();
      _in.next();
      _skip(de);
      de->expr = parse_unary_expr(de);
      de->fin = de->expr->fin;
      e = de;
      break;
   }      
   case Token::MinusMinus:
   case Token::PlusPlus: {
      IncrExpr *ie = new IncrExpr(tok.kind == Token::PlusPlus 
                                  ? IncrExpr::Positive 
                                  : IncrExpr::Negative);
      _in.consume(tok.kind == Token::PlusPlus ? "++" : "--");
      CommentSeq *comm = _in.skip("\n\t ");
      ie->expr = parse_unary_expr(ie);
      ie->preincr = true;
      ie->fin = ie->expr->fin;
      ie->comments.insert(ie->comments.begin(), comm);
      e = ie;
      break;
   }
   default:
      e = parse_postfix_expr(parent);
      break;
   }
   e->ini = ini;
   return e;
}

Expr *Parser::parse_expr(AstNode *parent, BinaryExpr::Kind max) {
   CommentSeq *cn;

   Expr *left = parse_unary_expr(parent);

   while (true) {
      Token tok = _in.peek_operator();
      if (!(tok.group & Token::Operator)) {
         break;
      }
      BinaryExpr::Kind kind = BinaryExpr::tok2kind(tok.kind);
      if (tok.kind == Token::Empty or kind > max) {
         break;
      }
      CommentSeq *c0 = _in.skip("\n\t ");
      tok = _in.read_operator();
      if (tok.kind == Token::QMark) { // (... ? ... : ...)
         CondExpr *e = new CondExpr();
         e->cond = left;
         e->comments.push_back(c0);
         _skip(e);
         e->then = parse_expr(e, Expr::Assignment); // Expr::comma?
         _skip(e);
         Token colon = _in.read_operator();
         if (colon.kind != Token::Colon) {
            error(e, _T("Expected '%s' here.", ":"));
         }
         _skip(e);
         e->els = parse_expr(e, Expr::Assignment);
         left = e;
      } else {
         BinaryExpr *e = new BinaryExpr();
         e->op = _in.substr(tok);
         e->set(kind);
         e->comments.push_back(c0);
         _skip(e);
         Expr::Kind submax = 
            Expr::Kind(Expr::right_associative(kind) 
                       ? kind 
                       : kind - 1);
         Expr *right = parse_expr(e, submax);
         e->left = left;
         e->right = right;
         e->ini = left->ini;
         e->fin = right->fin;
         left = e;
      }
   } 
   return left;
}

Expr *Parser::parse_callexpr(Expr *x) {
   CallExpr *e = new CallExpr();
   e->func = x;
   e->parent = x->parent;
   x->parent = e;
   _skip(e);
   _in.consume('(');
   _skip(e);
   if (_in.curr() != ')') {
      e->args.push_back(parse_expr(e, Expr::Assignment));
      _skip(e);
      while (_in.curr() == ',') {
         _in.next();
         _skip(e);
         e->args.push_back(parse_expr(e, Expr::Assignment));
         _skip(e);
      }
   }
   if (!_in.expect(")")) {
      error(e, _in.pos().str() + ": " + _T("Expected '%s' here.", ")"));
   }
   e->fin = _in.pos();
   return e;
}

Expr *Parser::parse_indexexpr(Expr *x) {
   IndexExpr *e = new IndexExpr();
   e->ini = x->ini;
   e->base = x;
   _in.consume('[');
   e->index = parse_expr(e);
   if (!_in.expect("]")) {
      error(e, _in.pos().str() + ": " + _T("Expected '%s' here.", "]"));
   }
   e->fin = _in.pos();
   _skip(e);
   return e;
}

Expr *Parser::parse_fieldexpr(Expr *x, Token tok) {
   FieldExpr *e = new FieldExpr();
   e->ini  = x->ini;
   e->base = x;
   e->pointer = (tok.kind == Token::Arrow);
   _in.consume(tok.kind == Token::Arrow ? "->" : ".");
   _skip(e);
   Token id = _in.read_id();
   e->field = new SimpleIdent(id.str);
   e->fin = _in.pos();
   return e;
}

Expr *Parser::parse_increxpr(Expr *x, Token tok) {
   IncrExpr *e = new IncrExpr(tok.kind == Token::PlusPlus 
                              ? IncrExpr::Positive 
                              : IncrExpr::Negative);
   e->expr = x;
   _in.consume(tok.kind == Token::PlusPlus ? "++" : "--");
   e->fin = _in.pos();
   return e;
}

Stmt *Parser::parse_for(AstNode *parent) {
   ForStmt *stmt = new ForStmt();
   stmt->parent = parent;
   _in.consume("for");
   _skip(stmt);
   if (!_in.expect("(")) {
      error(stmt, _in.pos().str() + ": " + _T("Expected '%s' here.", "("));
      // FIXME: resync?
   }
   _skip(stmt);
   if (_in.curr() == ';') {
      _in.next();
      stmt->init = 0;
   } else {
      stmt->init = parse_decl_or_expr_stmt(stmt);
   }
   _skip(stmt);
   if (_in.curr() == ';') {
      stmt->cond = 0;
   } else {
      stmt->cond = parse_expr(stmt);
   }
   _skip(stmt);
   if (!_in.expect(";")) {
      error(stmt, _in.pos().str() + ": " + _T("Expected '%s' here.", ";"));
   }
   _skip(stmt);
   if (_in.curr() == ')') {
      stmt->post = 0;
   } else {
      stmt->post = parse_expr(stmt);
   }
   if (!_in.expect(")")) {
      error(stmt, _in.pos().str() + ": " + _T("Expected '%s' here.", ")"));
   }
   _skip(stmt);
   stmt->substmt = parse_stmt(stmt);
   return stmt;
}

Stmt *Parser::parse_while(AstNode *parent) {
   WhileStmt *stmt = new WhileStmt();
   stmt->parent = parent;
   _in.consume("while");
   _skip(stmt);
   if (!_in.expect("(")) {
      error(stmt, _in.pos().str() + ": " + _T("Expected '%s' here.", "("));
   }
   _skip(stmt);
   stmt->cond = parse_expr(stmt);
   _skip(stmt);
   if (!_in.expect(")")) {
      error(stmt, _in.pos().str() + ": " + _T("Expected '%s' here.", ")"));
   }
   _skip(stmt);
   stmt->substmt = parse_stmt(stmt);
   return stmt;
}

Stmt *Parser::parse_ifstmt(AstNode *parent) {
   IfStmt *stmt = new IfStmt();
   stmt->parent = parent;
   _in.consume("if");
   _skip(stmt);
   if (!_in.expect("(")) {
      error(stmt, _in.pos().str() + ": " + _T("Expected '%s' here.", "("));
   }
   _skip(stmt);
   stmt->cond = parse_expr(stmt);
   if (!_in.expect(")")) {
      error(stmt, _in.pos().str() + ": " + _T("Expected '%s' here.", ")"));
   }
   _skip(stmt);
   stmt->then = parse_stmt(stmt);
   _in.save();
   _skip(stmt);
   string tok;
   if (_in.peek_token().kind == Token::Else) {
      _in.consume("else");
      _in.discard();
      _skip(stmt);
      stmt->els = parse_stmt(stmt);
   } else {
      stmt->comments.pop_back();
      _in.restore();
   }
   return stmt;
}

Stmt *Parser::parse_switch(AstNode *parent) {
   return error<Stmt>(_T("UNIMPLEMENTED"));
}

void Parser::parse_expr_seq(AstNode *parent, vector<Expr*>& exprs) {
   exprs.push_back(parse_expr(parent, Expr::Assignment));
   while (_in.curr() == ',') {
      _in.next();
      _skip(parent);
      exprs.push_back(parse_expr(parent, Expr::Assignment));
   }
}

void Parser::parse_type_seq(AstNode *parent, vector<TypeSpec*>& v) {
   v.push_back(parse_typespec(parent));
   _skip(parent);
   while (_in.curr() == ',') {
      _in.next();
      _skip(parent);
      v.push_back(parse_typespec(parent));
   }
}

Expr *Parser::parse_exprlist(AstNode *parent) {
   assert(_in.curr() == '{');
   ExprList *elist = new ExprList();
   do {
      _in.next();
      _skip(elist);
      if (_in.curr() == '}') {
         break;
      }
      elist->exprs.push_back(_in.curr() == '{' 
                             ? parse_exprlist(parent) 
                             : parse_expr(parent, Expr::Assignment));
   } while (_in.curr() == ',');

   if (!_in.expect("}")) {
      error(elist, _in.pos().str() + ": " + _T("Expected '%s' here.", "}"));
      _in.skip_to("},;\n");
   }
   _skip(elist);
   return elist;
}

Decl *Parser::_parse_vardecl(AstNode *parent, string name, Decl::Kind kind, CommentSeq *comm) {
   VarDecl *decl = new VarDecl();
   decl->parent = parent;
   decl->name = name;
   decl->kind = kind;
   decl->comments.push_back(comm);
   return decl;
}

Decl *Parser::_parse_arraydecl(AstNode *parent, string name, Decl::Kind kind, CommentSeq *comm) {
   ArrayDecl *decl = new ArrayDecl();
   decl->comments.push_back(comm);
   decl->parent = parent;
   decl->name = name;
   decl->kind = kind;
   while (_in.curr() == '[') {
      _in.consume("[");
      _skip(decl);
      decl->sizes.push_back(parse_expr(decl, Expr::Conditional));
      if (!_in.expect("]")) {
         error(decl, _in.pos().str() + ": " + _T("Expected '%s' here.", "]"));
      }
      _skip(decl);
   }
   return decl;
}

Decl *Parser::_parse_objdecl(AstNode *parent, string name, CommentSeq *comm) {
   _in.consume("(");
   ObjDecl *decl = new ObjDecl();
   decl->comments.push_back(comm);
   decl->parent = parent;
   decl->name = name;
   _skip(decl);
   parse_expr_seq(decl, decl->args);
   if (!_in.expect(")")) {
      error(decl, _in.pos().str() + ": " + _T("Expected '%s' here.", ")"));
      _in.skip_to("),;\n");
   }
   return decl;
}

DeclStmt *Parser::parse_declstmt(AstNode *parent, bool is_typedef) {
   DeclStmt *stmt = new DeclStmt();
   stmt->parent = parent;
   stmt->ini = _in.pos();
   TypeSpec *typespec = parse_typespec(stmt);
   _skip(stmt); // before identifier
   stmt->typespec = typespec;
   while (true) {
      Pos item_ini = _in.pos();
      Token id = _in.next_token();
      string name = id.str;
      Decl::Kind kind = Decl::Normal;
      if (id.kind == Token::Star) {
         kind = Decl::Pointer;
         _skip(stmt);
         id = _in.next_token();
         name = id.str;
      }
      if (id.group != Token::Ident) {
         error(stmt, _T("Expected an identifier here."));
      }
      DeclStmt::Item item;
      CommentSeq *comm = _in.skip("\n\t ");
      if (_in.curr() == '(' and !is_typedef) {
         item.decl = _parse_objdecl(stmt, name, comm);
      } else if (_in.curr() == '[') {
         item.decl = _parse_arraydecl(stmt, name, kind, comm);
      } else {
         item.decl = _parse_vardecl(stmt, name, kind, comm);
      }
      item.decl->ini = item_ini;
      if (_in.curr() == '=') {
         _in.next();
         _skip(stmt);
         item.init = (_in.curr() == '{' 
                      ? parse_exprlist(item.decl) 
                      : parse_expr(item.decl, Expr::Assignment));
      }
      item.decl->typespec = stmt->typespec;
      item.decl->fin = _in.pos();
      stmt->items.push_back(item);
      if (_in.curr() != ',' or is_typedef) {
         break;
      }
      _in.consume(",");
      _skip(stmt); // before identifier
   }
   stmt->fin = _in.pos();
   if (!_in.expect(";")) {
      error(stmt, _in.pos().str() + ": " + _T("Expected '%s' here.", ";"));
   }
   return stmt;
}

EnumDecl *Parser::parse_enum(AstNode *parent) {
   _in.consume("enum");
   EnumDecl *decl = new EnumDecl();
   decl->parent = parent;
   _skip(decl);
   Token tok = _in.next_token();
   if (!(tok.group & Token::Ident)) {
      error(decl, _in.pos().str() + ": " + _T("Expected an identifier here."));
      _in.skip_to(";");
      return decl;
   }
   decl->name = tok.str;
   _skip(decl);
   if (!_in.expect("{")) {
      error(decl, _in.pos().str() + ": " + _T("Expected '%s' here.", "{"));
      _in.skip_to("};");
   }
   _skip(decl);
   while (true) {
      Token tok = _in.next_token();
      if (!(tok.group & Token::Ident)) {
         error(decl, _in.pos().str() + ": " + _T("Expected an identifier here."));
         _in.skip_to(",}");
         break;
      }
      EnumDecl::Value v(tok.str);
      _skip(decl);
      if (_in.curr() == '=') {
         _in.next();
         _skip(decl);
         Token num = _in.read_number_literal();
         if (num.kind != Token::IntLiteral) {
            error(decl, _in.pos().str() + ": " + _T("Expected an integer here."));
            _in.skip_to(",};");
         }
         v.has_val = true;
         istringstream S(num.str);
         S >> v.val;
         _skip(decl);
      }
      decl->values.push_back(v);
      if (_in.curr() == ',') {
         _in.next();
         _skip(decl);
      } else {
         break;
      }
   }
   if (!_in.expect("}")) {
      error(decl, _in.pos().str() + ": " + _T("Expected '%s' here.", "{"));
   }
   if (!_in.expect(";")) {
      error(decl, _in.pos().str() + ": " + _T("Expected '%s' here.", ";"));
   }
   return decl;
}

TypedefDecl *Parser::parse_typedef(AstNode *parent) {
   _in.consume("typedef");
   TypedefDecl *typdef = new TypedefDecl();
   typdef->parent = parent;
   _skip(typdef);
   DeclStmt *stmt = parse_declstmt(typdef, true);    // FIXME: Y si hay varias declaraciones??
   typdef->decl = stmt->items[0].decl;
   for (CommentSeq *c : stmt->comments) {
      typdef->comments.push_back(c);
   }
   delete stmt;
   return typdef;
}

StructDecl *Parser::parse_struct(AstNode *parent) {
   Token tok = _in.next_token();
   assert(tok.kind == Token::Struct);

   StructDecl *decl = new StructDecl();
   decl->parent = parent;
   _skip(decl);

   decl->id = new SimpleIdent(_in.read_id().str);
   _skip(decl);
   
   tok = _in.next_token();
   if (tok.kind != Token::LCurly) {
      error(decl, _T("Expected '%s' here.", "{"));
      _in.skip_to("};");
      return decl;
   }
   _skip(decl);
   
   tok = _in.peek_token();
   while (!_in.end() and tok.kind != Token::RCurly) {
      DeclStmt *field = parse_declstmt(decl);
      decl->decls.push_back(field);
      field->parent = decl;
      _skip(decl);
      tok = _in.peek_token();      
   }
   if (tok.kind != Token::RCurly) {
      error(decl, _in.pos().str() + ": " + _T("Expected '%s' here.", "}"));
   }
   _in.expect("}");
   _skip(decl);
   if (!_in.expect(";")) {
      error(decl, _in.pos().str() + ": " + _T("Expected '%s' here.", ";"));
   }
   return decl;   
}
