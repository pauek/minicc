
#include <assert.h>
#include <sstream>
using namespace std;

#include "input.hh"

string Pos::str() const {
   ostringstream out;
   out << lin << ':' << col;
   return out.str();
}

void Input::skip_space() {
   while (!end() and (curr() == ' ' || curr() == '\t')) {
      next();
   }
}

void Input::skip_space_ln() {
   while (!end()) {
      switch (curr()) {
      case ' ': case '\t': 
         next();
      case '\n': 
         next(); 
         return;
      default:
         return;
      }
   }
}

void Input::skip_to(char stop) {
   while (!end() and curr() != stop) {
      next();
   }
}

void Input::skip_to_next_line() {
   skip_to('\n');
   next();
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
