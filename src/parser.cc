#include <cstdlib>
#include <sstream>
#include <fstream>
#include <assert.h>
using namespace std;

#include "parser.hh"
#include "translator.hh"

Parser::Parser(istream *i, std::ostream* err) : _lexer(i), _err(err) {
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

void Parser::error(Ast *n, string msg) {
   Error *err = new Error(n->ini, n->fin, msg);
   n->errors.push_back(err);
}

void Parser::error(Ast *n, Pos ini, Pos fin, string msg) {
   Error *err = new Error(ini, fin, msg);
   n->errors.push_back(err);
}

void Parser::stopper_error(Ast *n, string msg) {
   Error *err = new Error(n->ini, n->fin, msg);
   err->stopper = true;
   n->errors.push_back(err);
}

void Parser::stopper_error(Ast *n, Pos ini, Pos fin, string msg) {
   Error *err = new Error(ini, fin, msg);
   err->stopper = true;
   n->errors.push_back(err);
}

template<class Node>
typename Node::Error *Parser::error(string msg) {
   typename Node::Error *s = new typename Node::Error();
   s->code = _lexer.skip_to(";");
   error(s, msg);
   return s;
}

void Parser::fatal_error(Pos pos, string msg) {
   throw ParseError(pos, msg);
}

Ast* Parser::parse() {
   Program *prog = new Program();
   if (!_lexer.next()) {
      error(prog, _T("Error when reading input"));
      return prog;
   }
   _skip(prog);
   while (!_lexer.end()) {
      Pos pos = _lexer.pos();
      Token tok = _lexer.peek_token();
      switch (tok.type) {
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
         _lexer.skip_to(";");
         break;
      }
      case Token::Empty: {
         ostringstream msg;
         msg << pos << ": " << _T("Unexpected character '%c'", _lexer.curr());
         prog->add(error<Stmt>(msg.str()));
         _lexer.read_token();
         break;
      }
      default:
         if (tok.IsIdent() or tok.IsTypeSpec()) {
            prog->add(parse_func_or_var(prog));
            break;
         }
         string s = _lexer.SubStr(tok);
         error(prog, _T("Unexpected '%s' here.", s.c_str()));
         _lexer.read_token();
         break;
      }
      _skip(prog);
   }
   return prog;
}

Ast* Parser::parse_macro(Ast *parent) {
   Pos ini = _lexer.pos();
   _lexer.consume(Token::Sharp);
   _lexer.skip(Lexer::Skip::SpaceTab); // comments between '#' and the macro name are gobbled up...
   Pos macro_ini = _lexer.pos();
   if (!_lexer.expect("include")) {
      Token tok = _lexer.read_ident();
      string macro_name = _lexer.SubStr(tok);
      _lexer.skip_to("\n");
      Pos macro_fin = _lexer.pos();
      _lexer.next();
      Macro *m = new Macro(_lexer.SubStr(macro_ini, macro_fin));
      m->ini = ini;
      m->fin = macro_fin;
      fatal_error(macro_fin, _T("Macro '#%s' unknown.", macro_name.c_str()));
      return m;
   }
   Include* inc = new Include();
   inc->ini = ini;
   _skip(inc);
   char open = _lexer.curr();
   if (open != '"' && open != '<') {
      error(inc, _lexer.pos().str() + ": " + _T("Expected '\"' or '<' here."));
      _lexer.skip_to("\n");
      return inc;
   }
   char close = (open == '"' ? '"' : '>');
   const bool is_global = (open == '<');
   string filename;
   _lexer.next();
   while (_lexer.curr() != close) {
      if (_lexer.curr() == '\n') {
         Pos fin = _lexer.pos();
         inc->fin = fin;
         fatal_error(fin, _T("'#include' missing closing '%c'.", close));
         break;
      }
      filename += _lexer.curr();
      Pos p = _lexer.pos();
      _lexer.next();
      if (_lexer.end()) {
         error(inc, p, _lexer.pos(), _T("'#include' missing closing '%c'", close));
         break;
      }
   }
   if (_lexer.curr() == close) {
      _lexer.next();
   }
   inc->filename = filename;
   inc->global = (close == '>');
   inc->ini = ini;
   inc->fin = _lexer.pos();
   return inc;
}

Ast* Parser::parse_using_declaration(Ast *parent) {
   Using *u = new Using();
   u->ini = _lexer.pos();
   _lexer.consume("using");
   _skip(u);
   if (!_lexer.expect(Token::Namespace)) {
      error(u, u->ini.str() + ": " + _T("Expected '%s' here.", "namespace"));
      _lexer.skip_to("\n");
      return u;
   }
   _skip(u);
   Token tok = _lexer.read_ident();
   u->namespc = _lexer.SubStr(tok);
   _skip(u);
   u->fin = _lexer.pos();
   if (!_lexer.expect(Token::SemiColon)) {
      error(u, u->fin.str() + ": " + _T("Expected '%s' here.", ";"));
   }
   return u;
}

FullIdent *Parser::parse_ident(Ast *parent, Token tok, Pos ini) {
   FullIdent *id = new FullIdent(_lexer.SubStr(tok));
   Pos fin = _lexer.pos();
   while (true) {
      tok = _lexer.peek_token();
      if (_is_type(id->name) and tok.type == Token::LT) { // template_id
         _skip(id);
         _lexer.consume("<");
         _skip(id);
         parse_type_seq(id, id->subtypes);
         _skip(id);
         if (_lexer.curr() != '>') { // Do NOT call read_token here, since it will return ">>"
            error(id, _T("Expected '%s' here.", ">"));
         } else {
            _lexer.next();
         }
         fin = _lexer.pos();
      }
      tok = _lexer.peek_token();
      if (tok.type != Token::ColonColon) {
         break;
      }
      _skip(id);
      _lexer.consume("::");
      _skip(id);
      tok = _lexer.read_token();
      if (!tok.IsIdent()) {
         error(id, _T("Expected an identifier here"));
      }
      id->shift(_lexer.SubStr(tok));
      fin = _lexer.pos();
   }
   id->ini = ini;
   id->fin = fin;
   return id;
}

bool Parser::_parse_type_process_token(TypeSpec *type, Token tok, Pos p) {
   if (tok.IsBasicType()) {
      if (type->id != 0) {
         error(type, _T("Basic types are not templates"));
      }
      type->id = new FullIdent(_lexer.SubStr(tok));
      return true;
   } 
   if (tok.IsTypeQual()) {
      switch (tok.type) {
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
   if (type->id == 0 and tok.IsIdent()) {
      type->id = parse_ident(type, tok, p);
      return true;
   } 
   if (tok.type == Token::Amp) {
      type->reference = true;
      return true;
   }
   return false;
}

TypeSpec *Parser::parse_typespec(Ast *parent) {
   TypeSpec *type = new TypeSpec();
   type->parent = parent;

   Pos p = _lexer.pos();
   _lexer.save();
   Token tok = _lexer.read_token();
   while (_parse_type_process_token(type, tok, p)) {
      _lexer.discard();
      _lexer.save();
      _skip(type);
      p = _lexer.pos(), tok = _lexer.read_token();
   }
   _lexer.restore();
   return type;
}


Ast *Parser::parse_func_or_var(Ast *parent) {
   CommentSeq *c[2] = { 0, 0 };
   Pos ini = _lexer.pos();
   _lexer.save();
   TypeSpec *typespec = parse_typespec(0);
   c[0] = _lexer.skip();
   Pos id_ini = _lexer.pos();
   Token tok = _lexer.read_ident();
   FullIdent *id = parse_ident(0, tok, id_ini);
   c[1] = _lexer.skip();
   if (_lexer.curr() == '(') {
      _lexer.discard();
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
      _lexer.restore();
      return parse_declstmt(parent);
   }
   return NULL;
}

void Parser::parse_function(FuncDecl *fn) {
   CommentSeq *cn;

   // parameter list
   _lexer.consume('(');
   while (true) {
      _skip(fn);
      if (_lexer.curr() == ')') {
         break;
      }
      FuncDecl::Param *p = new FuncDecl::Param();
      p->ini = _lexer.pos();
      p->typespec = parse_typespec(fn);
      _skip(fn);
      Token tok = _lexer.read_ident();
      p->name = _lexer.SubStr(tok);
      _skip(fn);
      fn->params.push_back(p);
      p->fin = _lexer.pos();

      if (_lexer.curr() == ')') {
         break;
      } else if (_lexer.curr() == ',') {
         _lexer.consume(',');
      } else {
         error(fn, _T("Unexpected character '%c' in parameter list", _lexer.curr()));
         _lexer.skip_to(")");
      }
   }
   _lexer.consume(')');
   _skip(fn);
   if (_lexer.curr() == ';') {
      fn->block = 0;
      _lexer.next();
   } else {
      fn->block = parse_block(fn);
   }
   fn->fin = _lexer.pos();
}

Block *Parser::parse_block(Ast *parent) {
   Block *block = new Block();
   block->parent = parent;
   block->ini = _lexer.pos();
   if (!_lexer.expect(Token::LBrace)) {
      error(block, _T("I expected a '%s' here.", "{"));
      return block;
   }
   _skip(block);
   bool closing_curly = false;
   while (!_lexer.end()) {
      if (_lexer.curr() == '}') {
         closing_curly = true;
         _lexer.next();
         break;
      }
      Stmt *stmt = parse_stmt(block);
      block->stmts.push_back(stmt);
      _skip(block);
   }
   if (!closing_curly) {
      error(block, _T("Expected '}' but end of text found"));
   }
   block->fin = _lexer.pos();
   return block;
}

Stmt* Parser::parse_stmt(Ast *parent) {
   Token tok = _lexer.peek_token();
   switch (tok.type) {
   case Token::LParen:
      return parse_exprstmt(parent);

   case Token::LBrace:  
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
      if (tok.IsOperator()) {
         return parse_exprstmt(parent);
      }
      Stmt *stmt = parse_decl_or_expr_stmt(parent);
      if (stmt == 0) {
         _lexer.skip_to(";");
      }
      return stmt;
   }
}

Stmt *Parser::parse_decl_or_expr_stmt(Ast *parent) {
   _lexer.save();
   DeclStmt *declstmt;
   declstmt = parse_declstmt(parent);
   if (!declstmt->has_errors()) {
      _lexer.discard();
      return declstmt;
   }
   _lexer.restore();
   _lexer.save();
   ExprStmt *exprstmt = parse_exprstmt(parent);
   if (!exprstmt->has_errors()) {
      delete declstmt;
      _lexer.discard();
      return exprstmt;
   }
   _lexer.restore();
   // both have errors, return 0
   delete exprstmt;
   return 0;
}

Stmt *Parser::parse_jumpstmt(Ast *parent) {
   JumpStmt *stmt = new JumpStmt();
   stmt->parent = parent;
   stmt->ini = _lexer.pos();
   Token tok = _lexer.read_token();
   switch (tok.type) {
   case Token::Break:    stmt->kind = JumpStmt::Break; break;
   case Token::Continue: stmt->kind = JumpStmt::Continue; break;
   case Token::Goto:     stmt->kind = JumpStmt::Goto; break;
   default:
      break;
   }
   _skip(stmt);
   if (stmt->kind == JumpStmt::Goto) {
      Token tok = _lexer.read_ident();
      stmt->label = _lexer.SubStr(tok);
      _skip(stmt);
   }
   if (!_lexer.expect(Token::SemiColon)) {
      error(stmt, _lexer.pos().str() + ": " 
            + _T("Esperaba un ';' después de '%s'.", _lexer.SubStr(tok).c_str()));
      _lexer.skip_to(";\n"); // resync...
   }
   return stmt;
}

ExprStmt *Parser::parse_exprstmt(Ast *parent, bool is_return) {
   ExprStmt *stmt = new ExprStmt();
   stmt->parent = parent;
   stmt->ini = _lexer.pos();
   if (is_return) {
      Token tok = _lexer.read_token();
      assert(tok.type == Token::Return);
      _skip(stmt);
   }
   Pos eini = _lexer.pos();
   stmt->expr = (_lexer.curr() == ';' ? 0 : parse_expr(stmt));
   Pos efin = _lexer.pos();
   if (stmt->expr) {
      stmt->expr->ini = eini;
      stmt->expr->fin = efin;
   }
   _skip(stmt);
   stmt->fin = _lexer.pos();
   if (!_lexer.expect(Token::SemiColon)) {
      error(stmt, _lexer.pos().str() + ": " + _T("Expected ';' after expression"));
      _lexer.skip_to(";\n"); // resync...
   }
   return stmt;
}

Expr *Parser::parse_primary_expr(Ast *parent) {
   Expr *e;
   Pos ini = _lexer.pos();
   Token tok = _lexer.read_token();
   switch (tok.type) {
   case Token::LParen: {
      CommentSeq *cn = _lexer.skip();
      e = parse_expr(parent);
      e->paren = true;
      e->comments.insert(e->comments.begin(), cn);
      _skip(e);
      if (!_lexer.expect(Token::RParen)) {
         error(e, _lexer.pos().str() + ": Expected ')'");
      }
      e->ini = ini;
      e->fin = _lexer.pos();
      break;
   }
   case Token::True:
   case Token::False: {
      Literal* lit = new Literal(Literal::Bool);
      lit->parent = parent;
      lit->val.as_bool = (tok.type == Token::True);
      lit->ini = ini;
      lit->fin = _lexer.pos();
      _skip(lit);
      e = lit;
      break;
   }
   case Token::IntLiteral: {
      Literal* lit = new Literal(Literal::Int);
      lit->parent = parent;
      lit->val.as_int = atoi(_lexer.SubStr(tok).c_str());
      lit->ini = ini;
      lit->fin = _lexer.pos();
      _skip(lit);
      e = lit;
      break;
   }
   case Token::CharLiteral: {
      Literal* lit = new Literal(Literal::Char);
      lit->parent = parent;
      lit->val.as_char = _translate_escapes(_lexer.SubStr(tok))[0];
      lit->ini = ini;
      lit->fin = _lexer.pos();
      _skip(lit);
      e = lit;
      break;
   }
   case Token::Dot:
   case Token::FloatLiteral:
   case Token::DoubleLiteral: {
      Literal::Type typ = Literal::Double;
      if (tok.type == Token::FloatLiteral) {
         typ = Literal::Float;
      }
      Literal* lit = new Literal(typ);
      istringstream S(_lexer.SubStr(tok));
      S >> lit->val.as_double;
      lit->parent = parent;
      lit->ini = ini;
      lit->fin = _lexer.pos();
      _skip(lit);
      e = lit;
      break;
   }
   case Token::StringLiteral: {
      Literal* lit = new Literal(Literal::String);
      lit->parent = parent;
      lit->val.as_string.s = new string(_translate_escapes(_lexer.SubStr(tok))); // FIXME: Shouldn't copy string
      lit->ini = ini;
      lit->fin = _lexer.pos();
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

string Parser::_translate_escapes(string s) {
   string result;
   for (int i = 0; i < s.size(); i++) {
      if (s[i] == '\\') {
         i++;
         assert(i < s.size());
         switch (s[i]) {
         case 'a':  result += '\a'; break;
         case 'b':  result += '\b'; break;
         case 'f':  result += '\f'; break;
         case 'n':  result += '\n'; break;
         case 'r':  result += '\r'; break;
         case 't':  result += '\t'; break;
         case 'v':  result += '\v'; break;
         case '\'': result += '\''; break;
         case '\"': result += '\"'; break;
         case '\?': result += '\?'; break;
         case '\\': result += '\\'; break;
         default: 
            // FIXME: Don't know where to handle this error...
            cerr << "warning: unknown escape sequence '\\" 
                 << s[i] << "'" << endl;
            assert(false);
         }
      } else {
         result += s[i];
      }
   }
   return result;
}

Expr *Parser::parse_postfix_expr(Ast *parent, Expr *e = 0) {
   if (e == 0) {
      e = parse_primary_expr(parent);
   }
 begin:
   Token tok = _lexer.peek_token();
   switch (tok.type) {
   case Token::LParen:
      e = parse_callexpr(e);
      goto begin;
      
   case Token::LBracket:
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

Expr *Parser::parse_unary_expr(Ast *parent) {
   Expr *e;
   Pos ini = _lexer.pos();
   Token tok = _lexer.peek_token();
   switch (tok.type) {
   case Token::Not: {
      NegExpr *ne = new NegExpr();
      _lexer.next();
      _skip(ne);
      ne->expr = parse_unary_expr(ne);
      ne->fin = _lexer.pos();
      e = ne;
      break;
   }
   case Token::Plus:
   case Token::Minus: {
      SignExpr *se = new SignExpr(tok.type == Token::Plus
                                  ? SignExpr::Positive
                                  : SignExpr::Negative);
      _lexer.next();
      _skip(se);
      se->expr = parse_unary_expr(se);
      se->fin = se->expr->fin;
      e = se;
      break;
   }
   case Token::Amp: {
      AddrExpr *ae = new AddrExpr();
      _lexer.next();
      _skip(ae);
      ae->expr = parse_unary_expr(ae);
      ae->fin = ae->expr->fin;
      e = ae;
      break;
   }      
   case Token::Star: {
      DerefExpr *de = new DerefExpr();
      _lexer.next();
      _skip(de);
      de->expr = parse_unary_expr(de);
      de->fin = de->expr->fin;
      e = de;
      break;
   }      
   case Token::MinusMinus:
   case Token::PlusPlus: {
      IncrExpr *ie = new IncrExpr(tok.type == Token::PlusPlus 
                                  ? IncrExpr::Positive 
                                  : IncrExpr::Negative);
      _lexer.consume(tok.type == Token::PlusPlus ? "++" : "--");
      CommentSeq *comm = _lexer.skip();
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

Expr *Parser::parse_expr(Ast *parent, BinaryExpr::Kind max) {
   CommentSeq *cn;

   Expr *left = parse_unary_expr(parent);

   while (true) {
      Token tok = _lexer.peek_token();
      if (!tok.IsOperator()) {
         break;
      }
      BinaryExpr::Kind kind = BinaryExpr::tok2kind(tok.type);
      if (tok.type == Token::Empty or kind > max) {
         break;
      }
      CommentSeq *c0 = _lexer.skip();
      tok = _lexer.read_token();
      if (!tok.IsOperator()) {
         error(left, _T("Expected operator here."));
      }
      if (tok.type == Token::QMark) { // (... ? ... : ...)
         CondExpr *e = new CondExpr();
         e->cond = left;
         e->comments.push_back(c0);
         _skip(e);
         e->then = parse_expr(e, Expr::Eqment); // Expr::comma?
         _skip(e);
         Token colon = _lexer.read_token();
         if (colon.type != Token::Colon) {
            error(e, _T("Expected '%s' here.", ":"));
         }
         _skip(e);
         e->els = parse_expr(e, Expr::Eqment);
         left = e;
      } else {
         BinaryExpr *e = new BinaryExpr();
         e->op = _lexer.SubStr(tok);
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
   _lexer.consume('(');
   _skip(e);
   if (_lexer.curr() != ')') {
      e->args.push_back(parse_expr(e, Expr::Eqment));
      _skip(e);
      while (_lexer.curr() == ',') {
         _lexer.next();
         _skip(e);
         e->args.push_back(parse_expr(e, Expr::Eqment));
         _skip(e);
      }
   }
   if (!_lexer.expect(Token::RParen)) {
      error(e, _lexer.pos().str() + ": " + _T("Expected '%s' here.", ")"));
   }
   e->fin = _lexer.pos();
   return e;
}

Expr *Parser::parse_indexexpr(Expr *x) {
   IndexExpr *e = new IndexExpr();
   e->ini = x->ini;
   e->base = x;
   _lexer.consume('[');
   if (_lexer.curr() != ']') {
      e->index = parse_expr(e);
      if (!_lexer.expect(Token::RBracket)) {
         error(e, _lexer.pos().str() + ": " + _T("Expected '%s' here.", "]"));
      }
   } else {
      fatal_error(_lexer.pos(), _T("Debe haber una expresión entre los corchetes."));
      _lexer.consume(']');
   }
   e->fin = _lexer.pos();
   _skip(e);
   return e;
}

Expr *Parser::parse_fieldexpr(Expr *x, Token tok) {
   FieldExpr *e = new FieldExpr();
   e->ini  = x->ini;
   e->base = x;
   e->pointer = (tok.type == Token::Arrow);
   _lexer.consume(tok.type == Token::Arrow ? "->" : ".");
   _skip(e);
   Token id = _lexer.read_ident();
   e->field = new SimpleIdent(_lexer.SubStr(id));
   e->fin = _lexer.pos();
   return e;
}

Expr *Parser::parse_increxpr(Expr *x, Token tok) {
   IncrExpr *e = new IncrExpr(tok.type == Token::PlusPlus 
                              ? IncrExpr::Positive 
                              : IncrExpr::Negative);
   e->expr = x;
   _lexer.consume(tok.type == Token::PlusPlus ? "++" : "--");
   e->fin = _lexer.pos();
   return e;
}

bool wrong_for_with_commas(string code) {
   vector<int> commas, colons;
   for (int i = 0; i < code.size(); i++) {
      if (code[i] == ',') commas.push_back(i);
      else if (code[i] == ';') colons.push_back(i);
   }
   return commas.size() == 2 and colons.size() == 0;
}

Stmt *Parser::parse_for(Ast *parent) {
   ForStmt *stmt = new ForStmt();
   stmt->parent = parent;
   _lexer.consume("for");
   _skip(stmt);
   if (!_lexer.expect(Token::LParen)) {
      error(stmt, _lexer.pos().str() + ": " + _T("Expected '%s' here.", "("));
      // FIXME: resync?
   }
   _skip(stmt);
   if (_lexer.curr() == ';') {
      _lexer.next();
      stmt->init = 0;
   } else {
      stmt->init = parse_decl_or_expr_stmt(stmt);
      if (stmt->init == 0) {
         stmt->ini = _lexer.pos();
         string wrong_code = _lexer.skip_to(")");
         stmt->fin = _lexer.pos();
         if (wrong_for_with_commas(wrong_code)) {
            error(stmt, _T("El 'for' debe tener como separador el caracter ';' (y no ',')."));
         } else {
            error(stmt, _T("'for' erróneo."));
         }
         goto finish_for;
      }
   }
   _skip(stmt);
   if (_lexer.curr() == ';') {
      _lexer.next();
      stmt->cond = 0;
   } else {
      stmt->cond = parse_expr(stmt);
   }
   _skip(stmt);
   if (!_lexer.expect(Token::SemiColon)) {
      error(stmt, _lexer.pos().str() + ": " + _T("Expected '%s' here.", ";"));
   }
   _skip(stmt);
   if (_lexer.curr() == ')') {
      stmt->post = 0;
   } else {
      stmt->post = parse_expr(stmt);
   }
 finish_for:
   if (!_lexer.expect(Token::RParen)) {
      error(stmt, _lexer.pos().str() + ": " + _T("Expected '%s' here.", ")"));
   }
   _skip(stmt);
   stmt->substmt = parse_stmt(stmt);
   return stmt;
}

Stmt *Parser::parse_while(Ast *parent) {
   WhileStmt *stmt = new WhileStmt();
   stmt->parent = parent;
   _lexer.consume("while");
   _skip(stmt);
   if (!_lexer.expect(Token::LParen)) {
      error(stmt, _lexer.pos().str() + ": " + _T("Expected '%s' here.", "("));
   }
   _skip(stmt);
   stmt->cond = parse_expr(stmt);
   _skip(stmt);
   if (!_lexer.expect(Token::RParen)) {
      error(stmt, _lexer.pos().str() + ": " + _T("Expected '%s' here.", ")"));
   }
   _skip(stmt);
   stmt->substmt = parse_stmt(stmt);
   return stmt;
}

Stmt *Parser::parse_ifstmt(Ast *parent) {
   IfStmt *stmt = new IfStmt();
   stmt->parent = parent;
   _lexer.consume("if");
   _skip(stmt);
   if (!_lexer.expect(Token::LParen)) {
      error(stmt, _lexer.pos().str() + ": " + _T("Expected '%s' here.", "("));
   }
   _skip(stmt);
   stmt->cond = parse_expr(stmt);
   if (!_lexer.expect(Token::RParen)) {
      error(stmt, _lexer.pos().str() + ": " + _T("Expected '%s' here.", ")"));
   }
   _skip(stmt);
   stmt->then = parse_stmt(stmt);
   _lexer.save();
   _skip(stmt);
   string tok;
   if (_lexer.peek_token().type == Token::Else) {
      _lexer.consume("else");
      _lexer.discard();
      _skip(stmt);
      stmt->els = parse_stmt(stmt);
   } else {
      stmt->comments.pop_back();
      _lexer.restore();
   }
   return stmt;
}

Stmt *Parser::parse_switch(Ast *parent) {
   return error<Stmt>(_T("UNIMPLEMENTED"));
}

void Parser::parse_expr_seq(Ast *parent, vector<Expr*>& exprs) {
   exprs.push_back(parse_expr(parent, Expr::Eqment));
   while (_lexer.curr() == ',') {
      _lexer.next();
      _skip(parent);
      exprs.push_back(parse_expr(parent, Expr::Eqment));
   }
}

void Parser::parse_type_seq(Ast *parent, vector<TypeSpec*>& v) {
   v.push_back(parse_typespec(parent));
   _skip(parent);
   while (_lexer.curr() == ',') {
      _lexer.next();
      _skip(parent);
      v.push_back(parse_typespec(parent));
   }
}

Expr *Parser::parse_exprlist(Ast *parent) {
   assert(_lexer.curr() == '{');
   ExprList *elist = new ExprList();
   do {
      _lexer.next();
      _skip(elist);
      if (_lexer.curr() == '}') {
         break;
      }
      elist->exprs.push_back(_lexer.curr() == '{' 
                             ? parse_exprlist(parent) 
                             : parse_expr(parent, Expr::Eqment));
   } while (_lexer.curr() == ',');

   if (!_lexer.expect(Token::RBrace)) {
      error(elist, _lexer.pos().str() + ": " + _T("Expected '%s' here.", "}"));
      _lexer.skip_to("},;\n");
   }
   _skip(elist);
   return elist;
}

Decl *Parser::_parse_vardecl(Ast *parent, string name, Decl::Kind kind, CommentSeq *comm) {
   VarDecl *decl = new VarDecl();
   decl->parent = parent;
   decl->name = name;
   decl->kind = kind;
   decl->comments.push_back(comm);
   return decl;
}

Decl *Parser::_parse_arraydecl(Ast *parent, string name, Decl::Kind kind, CommentSeq *comm) {
   ArrayDecl *decl = new ArrayDecl();
   decl->comments.push_back(comm);
   decl->parent = parent;
   decl->name = name;
   decl->kind = kind;
   while (_lexer.curr() == '[') {
      _lexer.consume("[");
      _skip(decl);
      decl->sizes.push_back(parse_expr(decl, Expr::Conditional));
      if (!_lexer.expect(Token::RBracket)) {
         error(decl, _lexer.pos().str() + ": " + _T("Expected '%s' here.", "]"));
      }
      _skip(decl);
   }
   return decl;
}

Decl *Parser::_parse_objdecl(Ast *parent, string name, CommentSeq *comm) {
   _lexer.consume("(");
   ObjDecl *decl = new ObjDecl();
   decl->comments.push_back(comm);
   decl->parent = parent;
   decl->name = name;
   _skip(decl);
   parse_expr_seq(decl, decl->args);
   if (!_lexer.expect(Token::RParen)) {
      error(decl, _lexer.pos().str() + ": " + _T("Expected '%s' here.", ")"));
      _lexer.skip_to("),;\n");
   }
   return decl;
}

DeclStmt *Parser::parse_declstmt(Ast *parent, bool is_typedef) {
   DeclStmt *stmt = new DeclStmt();
   stmt->parent = parent;
   stmt->ini = _lexer.pos();
   TypeSpec *typespec = parse_typespec(stmt);
   stmt->typespec = typespec;
   _skip(stmt); // before identifier
   Pos after_comma = _lexer.pos(), after_id = _lexer.pos();
   while (true) {
      Pos item_ini = _lexer.pos();
      Token id = _lexer.read_token();
      string name = _lexer.SubStr(id);
      Decl::Kind kind = Decl::Normal;
      if (id.type == Token::Star) {
         kind = Decl::Pointer;
         _skip(stmt);
         id = _lexer.read_token();
         name = _lexer.SubStr(id);
      }
      if (!id.IsIdent()) {
         stopper_error(stmt, _T("Expected a variable name here."));
      }
      after_id = _lexer.pos();
      DeclStmt::Item item;
      CommentSeq *comm = _lexer.skip();
      if (_lexer.curr() == '(' and !is_typedef) {
         item.decl = _parse_objdecl(stmt, name, comm);
      } else if (_lexer.curr() == '[') {
         item.decl = _parse_arraydecl(stmt, name, kind, comm);
      } else {
         item.decl = _parse_vardecl(stmt, name, kind, comm);
      }
      item.decl->ini = item_ini;
      if (_lexer.curr() == '=') {
         _lexer.next();
         _skip(stmt);
         item.init = (_lexer.curr() == '{' 
                      ? parse_exprlist(item.decl) 
                      : parse_expr(item.decl, Expr::Eqment));
      }
      item.decl->typespec = stmt->typespec;
      item.decl->fin = _lexer.pos();
      stmt->items.push_back(item);
      if (_lexer.curr() != ',' or is_typedef) {
         break;
      }
      _lexer.consume(",");
      after_comma = _lexer.pos();
      _skip(stmt); // before identifier
   }
   stmt->fin = _lexer.pos();
   if (!_lexer.expect(Token::SemiColon)) {
      stopper_error(stmt, _T("Expected '%s' here.", ";"));
   }
   return stmt;
}

EnumDecl *Parser::parse_enum(Ast *parent) {
   _lexer.consume("enum");
   EnumDecl *decl = new EnumDecl();
   decl->parent = parent;
   _skip(decl);
   Token tok = _lexer.read_token();
   if (!tok.IsIdent()) {
      error(decl, _lexer.pos().str() + ": " + _T("Expected an identifier here."));
      _lexer.skip_to(";");
      return decl;
   }
   decl->name = _lexer.SubStr(tok);
   _skip(decl);
   if (!_lexer.expect(Token::LBrace)) {
      error(decl, _lexer.pos().str() + ": " + _T("Expected '%s' here.", "{"));
      _lexer.skip_to("};");
   }
   _skip(decl);
   while (true) {
      Token tok = _lexer.read_token();
      if (!tok.IsIdent()) {
         error(decl, _lexer.pos().str() + ": " + _T("Expected an identifier here."));
         _lexer.skip_to(",}");
         break;
      }
      EnumDecl::Value v(_lexer.SubStr(tok));
      _skip(decl);
      if (_lexer.curr() == '=') {
         _lexer.next();
         _skip(decl);
         Token num = _lexer.read_number_literal();
         if (num.type != Token::IntLiteral) {
            error(decl, _lexer.pos().str() + ": " + _T("Expected an integer here."));
            _lexer.skip_to(",};");
         }
         v.has_val = true;
         istringstream S(_lexer.SubStr(num));
         S >> v.val;
         _skip(decl);
      }
      decl->values.push_back(v);
      if (_lexer.curr() == ',') {
         _lexer.next();
         _skip(decl);
      } else {
         break;
      }
   }
   if (!_lexer.expect(Token::RBrace)) {
      error(decl, _lexer.pos().str() + ": " + _T("Expected '%s' here.", "{"));
   }
   if (!_lexer.expect(Token::SemiColon)) {
      error(decl, _lexer.pos().str() + ": " + _T("Expected '%s' here.", ";"));
   }
   return decl;
}

TypedefDecl *Parser::parse_typedef(Ast *parent) {
   _lexer.consume("typedef");
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

StructDecl *Parser::parse_struct(Ast *parent) {
   Token tok = _lexer.read_token();
   assert(tok.type == Token::Struct);

   StructDecl *decl = new StructDecl();
   decl->parent = parent;
   _skip(decl);

   Token id = _lexer.read_ident();
   decl->id = new SimpleIdent(_lexer.SubStr(id));
   _skip(decl);
   
   tok = _lexer.read_token();
   if (tok.type != Token::LBrace) {
      error(decl, _T("Expected '%s' here.", "{"));
      _lexer.skip_to("};");
      return decl;
   }
   _skip(decl);
   
   tok = _lexer.peek_token();
   while (!_lexer.end() and tok.type != Token::RBrace) {
      DeclStmt *field = parse_declstmt(decl);
      decl->decls.push_back(field);
      field->parent = decl;
      _skip(decl);
      tok = _lexer.peek_token();      
   }
   if (tok.type != Token::RBrace) {
      error(decl, _lexer.pos().str() + ": " + _T("Expected '%s' here.", "}"));
   }
   _lexer.expect(Token::RBrace);
   _skip(decl);
   if (!_lexer.expect(Token::SemiColon)) {
      error(decl, _lexer.pos().str() + ": " + _T("Expected '%s' here.", ";"));
   }
   return decl;   
}
