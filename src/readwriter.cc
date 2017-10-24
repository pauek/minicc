
#include <iostream>
#include "readwriter.hh"

std::ostream& ReadWriter::out(OutType typ) { 
   std::ostream *o = _out;
   if (!_stack.empty()) {
      o = _stack.back();
   }
   if (typ == beginl and _indent > 0) {
      *o << indentation();
   }
   return *o; 
}
