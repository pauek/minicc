#ifndef INPUT_H
#define INPUT_H

#include <iostream>

struct Pos { 
   int lin, col; 
   Pos() : lin(1), col(0) {}
};

inline std::ostream& operator<<(std::ostream& o, const Pos& pos) {
   o << pos.lin << ":" << pos.col;
   return o;
}

class Input {
   std::istream* _in;
   std::string _text;
   int _curr;
   Pos _pos;

   void _reset() { 
      _pos.lin = 1;
      _pos.col = -1;
      _curr = -1; 
   }

public:
   Input()                : _in(0) { _reset(); }
   Input(std::istream* i) : _in(i) { _reset(); }
   
   Pos  pos() const { return _pos; }
   bool next();
   char curr() const { return _text[_curr]; }
};

#endif
