#ifndef INPUT_H
#define INPUT_H

#include <iostream>
#include <vector>

struct Pos { 
   int lin, col; 
   Pos() : lin(1), col(0) {}
   std::string str() const;
};

inline std::ostream& operator<<(std::ostream& o, const Pos& pos) {
   o << pos.lin << ":" << pos.col;
   return o;
}

inline bool operator==(const Pos& a, const Pos& b) {
   return a.lin == b.lin && a.col == b.col;
}

inline bool operator<(const Pos& a, const Pos& b) {
   if (a.lin != b.lin) {
      return a.lin < b.lin;
   } else {
      return a.col < b.col;
   }
}

inline bool operator>(const Pos& a, const Pos& b) {
   if (a.lin != b.lin) {
      return a.lin > b.lin;
   } else {
      return a.col > b.col;
   }
}

inline bool operator<=(const Pos& a, const Pos& b) { 
   return operator<(a, b) || operator==(a, b); 
}

inline bool operator>=(const Pos& a, const Pos& b) { 
   return operator>(a, b) || operator==(a, b); 
}

struct Range {
   Pos ini, fin;
   Range(Pos i, Pos f) : ini(i), fin(f) {}
};

// Input /////////////////////////////////////////////////////////////

class Input {
   std::istream* _in;
   std::string _text;
   int _curr;
   Pos _pos;
   std::vector<int> _linepos; // positions of line starts (ignoring position 0 since no line 0)

   void _reset() { 
      _pos.lin = 1;
      _pos.col = -1;
      _curr = -1; 
   }

   int _pos_to_idx(Pos p) const;

public:
   Input()                : _in(0), _linepos(1) { _reset(); }
   Input(std::istream* i) : _in(i), _linepos(1) { _reset(); }
   
   bool next();
   Pos  pos()  const { return _pos; }
   char curr() const { return _text[_curr]; }
   bool end()  const { return _curr >= _text.size(); }

   void skip_space();
   void skip_space_ln();
   void skip_to(char stop);
   void skip_to_next_line();
   bool expect(std::string word);

   std::string substr(const Pos& ini, const Pos& fin) const;
};

#endif
