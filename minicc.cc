#include <iostream>
#include <fstream>
using namespace std;

struct Pos { 
   int lin, col; 

   Pos() : lin(1), col(0) {}
};

ostream& operator<<(ostream& o, const Pos& pos) {
   o << pos.lin << ":" << pos.col;
   return o;
}

class Input {
   istream* _in;
   string _text;
   int _curr;
   Pos _pos;

   void _reset() { 
      _pos.lin = 1;
      _pos.col = -1;
      _curr = -1; 
   }

public:
   Input()           : _in(0) { _reset(); }
   Input(istream* i) : _in(i) { _reset(); }

   Pos  pos() const { return _pos; }
   bool next();
   char curr() const { return _text[_curr]; }
};

bool Input::next() {
   if (_in == 0) {
      return false;
   }
   _curr++;
   _pos.col++;
   while (1) {
      if (_curr >= _text.size()) {
         if (!_in->good()) {
            return false;
         }
         string line;
         getline(*_in, line);
         _text += line + "\n";
      }
      if (_text[_curr] == '\n') {
         _pos.lin++;
         _pos.col = 0;
         _curr++;
      } else {
         break;
      }
   }
   return true;
}

int main(int argc, char *argv[]) {
   istream *i = &cin;
   if (argc > 1) {
      i = new ifstream(argv[1]);
   }
   Input I(i);
   while (I.next()) {
      cout << "'" << I.curr() << "' " << I.pos() << endl;
   }
}
