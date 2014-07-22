
#include <assert.h>
#include <sstream>
using namespace std;

#include "input.hh"
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

void Input::error(string msg) {
   cerr << msg << endl;
   exit(1);
}

void Input::consume(char c) {
   assert(curr() == c);
   next();
}

void Input::consume(string word) {
   for (int i = 0; i < word.size(); i++) {
      consume(word[i]);
   }
}

string Input::substr(const Token& t) {
   string s;
   if (t.ini != -1 and t.fin != -1) {
      s = _text.substr(t.ini, t.fin - t.ini);
   } 
   return s;
}

string Input::substr(const Pos& ini, const Pos& fin) const {
   const int i = _pos_to_idx(ini);
   const int j = _pos_to_idx(fin);
   if (i == -1 || j == -1) {
      return "";
   }
   return _text.substr(i, j - i);
}

bool Input::curr_one_of(std::string set) const { 
   return set.find(curr()) != std::string::npos; 
}

CommentNode* Input::skip(string skip_set) {
   CommentNode *cn = 0;
   while (!end()) {
      if (curr() == '/') {
         peek(1);
         if (cn == 0) {
            cn = new CommentNode();
         }
         if (curr(1) == '*') {
            cn->comments.push_back(Comment(Comment::multiline));
            read_multiline_comment(cn->comments.back());
         } else if (curr(1) == '/') {
            cn->comments.push_back(Comment(Comment::singleline));
            read_singleline_comment(cn->comments.back());
         } else {
            break;
         }
      }
      if (cn != 0 and curr() == '\n') {
         cn->comments.back().endl = true;
      }
      if (skip_set.find(curr()) == string::npos) {
         break;
      }
      next();
   }
   return cn;
}

string Input::skip_to(string stop_set) {
   string s;
   while (!end() and (stop_set.find(curr()) == string::npos)) {
      s += curr();
      next();
   }
   return s;
}

void Input::save() {
   _stack.push_back(make_pair(_curr, _pos));
}

void Input::restore() {
   assert(!_stack.empty());
   pair<int, Pos> top = _stack.back();
   _curr = top.first;
   _pos  = top.second;
   _stack.pop_back();
}

void Input::discard() {
   _stack.pop_back();
}

string Input::peek_to(string stop_set) {
   save();
   string res = skip_to(stop_set);
   restore();
   return res;
}


Token Input::next_token() {
   switch (curr()) {
   case '.': case '(': case '[': case '{': case '#': {
      string s(1, curr());
      next();
      return Token(Token::token2type(s));
   }
   case '+': case '&': case '!':
   case '-':
   case '*': case '/': case '%': case '=': case '^': 
   case '<': case '>': 
   case ',': case '~': {
      return read_operator();
   }      
   case '0': case '1': case '2': case '3': case '4':
   case '5': case '6': case '7': case '8': case '9':
      return read_number_literal();
      
   case 'u': case 'U': case 'l':

   case 'L': // char-lit, string-lit, long
      switch (curr(1)) {
      case '\'': 
         return read_char_literal();
      case '"':  
         return read_string_literal();
      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
         return read_number_literal();
      default:
         string id = read_id();
         return Token(Token::token2type(id));
      }

   default:
      string id = read_id();
      return Token(Token::token2type(id));
   }
}

Token Input::peek_token() {
   save();
   Token tok = next_token();
   restore();
   return tok;
}

string Input::skip_to_next_line() {
   string s = skip_to("\n");
   next();
   return s;
}

int Input::_pos_to_idx(Pos p) const {
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

bool Input::peek(int offset) {
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

bool Input::next() {
   if (_in == 0 || !_in->good()) {
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

bool Input::expect(string word) {
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

void Input::read_singleline_comment(Comment& c) {
   consume("//");
   while (!end() and curr() != '\n') {
      c.text += curr();
      next();
   }
   return;
}

void Input::read_multiline_comment(Comment& c) {
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

string Input::read_id() {
   string id;
   char c = curr();
   if (!_isupper(c) and !_islower(c) and c != '_') {
      return "";
   }
   id += c;
   next();
   c = curr();
   while (_isupper(c) or _islower(c) or _isdigit(c) or c == '_') {
      id += c;
      next();
      c = curr();
   }
   return id;
}

Token Input::read_operator() {
   string op;
   char x;
   int ini = _curr, fin = -1;
   switch (curr()) {
   case '+': case '&': case '|': // + ++ += & && &= | || |=
      x = curr();
      op += x; next();
      if (curr() == '=' or curr() == x) {
         op += curr(); next();
      }
      break;

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

   case '*': case '/': case '%': // * *= / /= % %= = == ! != ^ ^=
   case '=': case '!': case '^': 
      op += curr(); next();
      if (curr() == '=') {
         op += '='; next();
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

   case ',': case '~':           // , ~
      op += curr(); next();
      break;
      
   default:
      break;
   }
   fin = _curr;
   Token t(Token::token2type(op));
   t.ini = ini;
   t.fin = fin;
   return t;
}

Token Input::read_string_literal() {
   error("UNIMPLEMENTED 1");
   return Token();
}

Token Input::read_number_literal() {
   string num;
   while (isdigit(curr())) {
      num += curr();
      next();
   }
   return Token(Token::IntLiteral);
}

Token Input::read_char_literal() {
   error("UNIMPLEMENTED 3");
   return Token();
}
