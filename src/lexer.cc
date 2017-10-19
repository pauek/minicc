
#include <assert.h>
#include <sstream>
using namespace std;

#include "lexer.hh"
#include "ast.hh"

bool is_space(string s) {
   for (char c : s) {
      if (c != ' ' && c != '\t') {
         return false;
      }
   }
   return true;
}

string Pos::str() const {
   ostringstream out;
   out << lin << ':' << col;
   return out.str();
}

void Pos::to_json(ostream& o) const {
   o << "{\"lin\": " << lin << ", \"col\": " << col << "}";
}

void Lexer::error(string msg) {
   cerr << msg << endl;
   exit(1);
}

void Lexer::consume(char c) {
   assert(curr() == c);
   next();
}

void Lexer::consume(string word) {
   for (int i = 0; i < word.size(); i++) {
      consume(word[i]);
   }
}

string Lexer::substr(const Token& t) {
   string s;
   if (t.ini != -1 and t.fin != -1) {
      s = _text.substr(t.ini, t.fin - t.ini);
   } 
   return s;
}

string Lexer::substr(const Pos& ini, const Pos& fin) const {
   const int i = _pos_to_idx(ini);
   const int j = _pos_to_idx(fin);
   if (i == -1 || j == -1) {
      return "";
   }
   return _text.substr(i, j - i);
}

bool Lexer::curr_one_of(std::string set) const { 
   return set.find(curr()) != std::string::npos; 
}

CommentSeq* Lexer::skip(string skip_set) {
   CommentSeq *cs = 0;
   int endls_in_a_row = 0;
   while (!end()) {
      while (curr() == '/') {
         peek(1);
         if (cs == 0) {
            cs = new CommentSeq();
         }
         if (curr(1) == '*') {
            cs->items.push_back(Comment(Comment::multiline));
            read_multiline_comment(cs->items.back());
            endls_in_a_row = 0;
         } else if (curr(1) == '/') {
            cs->items.push_back(Comment(Comment::singleline));
            read_singleline_comment(cs->items.back());
            endls_in_a_row = 0;
         } else {
            break;
         }
      }
      if (curr() == '\n') {
         endls_in_a_row++;
         if (cs == 0) {
            cs = new CommentSeq();
         }
         if (endls_in_a_row < 3) {
            cs->items.push_back(Comment(Comment::endline));
         }
      } else {
         endls_in_a_row = 0;
      }
      if (skip_set.find(curr()) == string::npos) {
         break;
      }
      next();
   }
   return cs;
}

string Lexer::skip_to(string stop_set) {
   string s;
   while (!end() and (stop_set.find(curr()) == string::npos)) {
      s += curr();
      next();
   }
   return s;
}

void Lexer::save() {
   _stack.push_back(SavedItem(_curr, _pos, _linepos));
}

void Lexer::restore() {
   assert(!_stack.empty());
   SavedItem item = _stack.back();
   _curr    = item.curr;
   _pos     = item.pos;
   _linepos = item.linepos;
   _stack.pop_back();
}

void Lexer::discard() {
   _stack.pop_back();
}

Token Lexer::next_token() {

#define RESULT1(type, group, s) do {\
   Token tok(Token::type, Token::group); \
   tok.ini = _curr; \
   next(); \
   tok.fin = _curr; \
   tok.str = s; \
   return tok; \
} while(0)

#define RESULT2(type, group, s) do {\
   Token tok(Token::type, Token::group); \
   tok.ini = _curr; \
   next(), next(); \
   tok.fin = _curr; \
   tok.str = s; \
   return tok; \
} while(0)

#define RESULT_1_2(ch, type1, group1, s1, type2, group2, s2) \
   if (curr(1) == ch) RESULT2(type2, group2, s2); \
   else RESULT1(type1, group1, s1); \

#define RESULT_OP_EQUALS(ch, str, type1, type2, type3) \
   if (curr(1) == ch) RESULT2(type2, Operator, str str); \
   else if (curr(1) == '=') RESULT2(type3, Operator, str "="); \
   else RESULT1(type1, Operator, str);


   switch (curr()) {
   case '(': RESULT1(LParen, None, "(");
   case ')': RESULT1(Unknown, Ident, ")");
   case '[': RESULT1(LBrack, None, "[");
   case ']': RESULT1(RBrack, None, "]");
   case '{': RESULT1(LCurly, None, "{");
   case '}': RESULT1(RCurly, None, "}");
   case ';': RESULT1(SemiColon, None, ";");
   case '#': RESULT1(Sharp, None, "#");
   case ',': RESULT1(Comma, Operator, ",");
   case '?': RESULT1(QMark, Operator, "?");

   // TODO: Add '~'
   // case '~': RESULT1(Tilde, Operator, "~");

   case ':': RESULT_1_2(':', Colon,      None,     ":", 
                             ColonColon, None,     "::");
   case '=': RESULT_1_2('=', Assign,     Operator, "=", 
                             EqEq,       Operator, "==");
   case '!': RESULT_1_2('=', Not,        Operator, "!",
                             NotEq,      Operator, "!=");
   case '*': RESULT_1_2('=', Star,       Operator, "*",
                             StarAssign, Operator, "*=");
   case '/': RESULT_1_2('=', Slash,       Operator, "/",
                             SlashAssign, Operator, "/=");
   case '%': RESULT_1_2('=', Percent,     Operator, "%",
                             DivAssign,   Operator, "%=");
   case '^': RESULT_1_2('=', Circum,      Operator, "^",
                             XorAssign,   Operator, "^=");

   case '+': RESULT_OP_EQUALS('+', "+", Plus, PlusPlus, PlusAssign);
   case '|': RESULT_OP_EQUALS('|', "|", Pipe, BarBar,   OrAssign);
   case '&': RESULT_OP_EQUALS('&', "&", Amp,  AmpAmp,   AndAssign);

   case '.': {
      if (isdigit(curr(1))) {
         return read_number_literal();
      }
      RESULT1(Dot, Operator, ".");
   }

   case '-': case '<': case '>': {
      return read_operator();
   }      
   case '0': case '1': case '2': case '3': case '4':
   case '5': case '6': case '7': case '8': case '9':
      return read_number_literal();
      
   case 'u': case 'U': case 'l':
   case 'L': // char-lit, string-lit, long
      switch (curr(1)) {
      case '\'': 
         return read_string_or_char_literal('\'');
      case '"':  
         return read_string_or_char_literal('"');
      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
         return read_number_literal();
      default:
         return read_id();
      }

   case '"':
      return read_string_or_char_literal('"');
      
   case '\'':
      return read_string_or_char_literal('\'');

   default: {
      return read_id();
   }
   }
}

Token Lexer::peek_token() {
   save();
   skip("\n\t ");
   Token tok = next_token();
   restore();
   return tok;
}

Token Lexer::peek_operator() {
   save();
   skip("\n\t ");
   Token tok = read_operator();
   restore();
   return tok;
}

int Lexer::_pos_to_idx(Pos p) const {
   if (p.lin < 1 || p.lin >= _linepos.size()) {
      return -1;
   }
   int lini = _linepos[p.lin];
   int lfin = _text.size();
   if (p.lin < _linepos.size()-1) {
      lfin = _linepos[p.lin+1];
   }
   const int lsize = lfin - lini;
   if (p.col < 0 || p.col >= lsize) {
      return -1;
   }
   return lini + p.col;
}

bool Lexer::peek(int offset) {
   if (_in == 0 || !_in->good()) {
      return false;
   }
   int k = _curr + offset;
   if (k >= _text.size()) {
      string line;
      if (!getline(*_in, line)) {
         return false;
      }
      _text += line;
      if (!_in->eof()) {
         _text += "\n";
      }
   }
   return k < _text.size();
}

bool Lexer::next() {
   if (_in == 0) {
      return false;
   }
   if (_curr == -1) {
      _pos.lin = 1;
      _pos.col = 0;
      _linepos.push_back(0);
   } else if (_curr < _text.size() && _text[_curr] == '\n') {
      _pos.lin++;
      _pos.col = 0;
      _linepos.push_back(_curr+1);
      _seen_endl = true;
   } else {
      _pos.col++;
   }
   _curr++;
   assert(_curr <= _text.size());
   if (_curr == _text.size()) {
      string line;
      if (!getline(*_in, line)) {
         return false;
      }
      _text += line;
      if (!_in->eof()) {
         _text += "\n";
      }
   }
   return true;
}

bool Lexer::expect(string word) {
   Pos p = _pos;
   int c = _curr;
   for (int i = 0; i < word.size(); i++) {
      if (curr() != word[i]) {
         _curr = c;
         _pos = p;
         return false;
      }
      next();
   }
   return true;
}

// read_*

void Lexer::read_singleline_comment(Comment& c) {
   consume("//");
   while (!end() and curr() != '\n') {
      c.text += curr();
      next();
   }
   return;
}

void Lexer::read_multiline_comment(Comment& c) {
   consume("/*");
   while (!end()) {
      if (curr() == '*') {
         peek(1);
         if (curr(1) == '/') {
            consume("*/");
            return;
         }
      }
      c.text += curr();
      next();
   }
   error(pos().str() + "unfinished comment");
   return;
}

inline bool _isupper(char c) { return c >= 'A' and c <= 'Z'; }
inline bool _islower(char c) { return c >= 'a' and c <= 'z'; }
inline bool _isdigit(char c) { return c >= '0' and c <= '9'; }

Token Lexer::read_id() {
   Token t;
   t.ini = _curr;
   char c = curr();
   if (!_isupper(c) and !_islower(c) and c != '_') {
      return Token();
   }
   t.str += c;
   next();
   c = curr();
   while (_isupper(c) or _islower(c) or _isdigit(c) or c == '_') {
      t.str += c;
      next();
      c = curr();
   }
   t.fin = _curr;
   Token x = Token::token2type(substr(t));
   t.type  = x.type;
   t.group = x.group;
   return t;
}

Token Lexer::read_operator() {
   string op;
   char x;
   int ini = _curr, fin = -1;
   switch (curr()) {
   case '-':                     // - -- -= -> ->*
      op += '-'; next();
      switch (curr()) {
      case '=': case '-':
         op += curr(); next();
         break;

      case '>':
         op += '>'; next();
         if (curr() == '*') {
            op += '*'; next();
         }
         break;
      }
      break;

   case '<': case '>':           // < <= << <<= > >= >> >>=
      x = curr();
      op += x; next();
      if (curr() == x) {
         op += x; next();
      } 
      if (curr() == '=') {
         op += '='; next();
      }
      break;

   case 'o':
      if (curr(1) == 'r') {
         next(), next();
         op = "or";
      }
      break;

   case 'a':
      if (curr(1) == 'n' and curr(2) == 'd') {
         next(), next(), next();
         op = "and";
      }
      break;
      
   default:
      break;
   }
   fin = _curr;
   Token t(Token::token2type(op));
   t.ini = ini;
   t.fin = fin;
   t.str = op;
   return t;
}

Token Lexer::read_string_or_char_literal(char delim) {
   string str;
   Token t;
   t.ini = _curr+1;
   if (curr() == 'L') {
      next(); // TODO: Handle 'L'
   }
   consume(delim);
   while (curr() != delim) {
      if (curr() == '\\') {
         next();
         switch (curr()) {
         case 'a':  t.str += '\a'; break;
         case 'b':  t.str += '\b'; break;
         case 'f':  t.str += '\f'; break;
         case 'n':  t.str += '\n'; break;
         case 'r':  t.str += '\r'; break;
         case 't':  t.str += '\t'; break;
         case 'v':  t.str += '\v'; break;
         case '\'': t.str += '\''; break;
         case '\"': t.str += '\"'; break;
         case '\?': t.str += '\?'; break;
         case '\\': t.str += '\\'; break;
         default: 
            cerr << "warning: unknown escape sequence '\\" 
                 << curr() << "'" << endl;
            t.str += curr();
         }
      } else if (curr() == '\n') {
         error(pos().str() + ": string inacabado");
         break;
      } else {
         t.str += curr();
      }
      next();
   }
   t.fin = _curr;
   if (curr() == delim) {
      consume(delim);
   }
   t.type = (delim == '"' ? Token::StringLiteral : Token::CharLiteral);
   return t;
}

Token Lexer::read_number_literal() {
   Token t;
   t.ini = _curr;
   if (curr() == '.') {
      next();
      t.str += '.';
      return read_real_literal(t);
   }
   while (isdigit(curr())) {
      t.str += curr();
      next();
   }
   if (curr() == '.') {
      next();
      t.str += '.';
      return read_real_literal(t);
   }
   t.fin = _curr;
   t.type = Token::IntLiteral;
   return t;
}

Token Lexer::read_real_literal(Token t) {
   while (isdigit(curr())) {
      t.str += curr();
      next();
   }
   bool isfloat = false;
   if (curr() == 'f') {
      isfloat = true;
      next();
   }
   t.fin = _curr;
   t.type = (isfloat 
             ? Token::FloatLiteral 
             : Token::DoubleLiteral);
   return t;
}
