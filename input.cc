
#include "input.h"
using namespace std;

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
