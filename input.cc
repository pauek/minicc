
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

string Input::skip_to(string stop_set) {
   string s;
   while (!end() and (stop_set.find(curr()) == string::npos)) {
      s += curr();
      next();
   }
   return s;
}

string Input::peek_to(string stop_set) {
   Pos saved_pos = _pos;
   int saved_curr = _curr;
   string res = skip_to(stop_set);
   _pos = saved_pos;
   _curr = saved_curr;
   return res;
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

string Input::substr(const Pos& ini, const Pos& fin) const {
   const int i = _pos_to_idx(ini);
   const int j = _pos_to_idx(fin);
   if (i == -1 || j == -1) {
      return "";
   }
   return _text.substr(i, j - i);
}
