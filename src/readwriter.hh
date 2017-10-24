
#ifndef READWRITER_HH
#define READWRITER_HH

#include <iostream>
#include <sstream>
#include <vector>
#include <assert.h>

class ReadWriter {
   int _indent;
   std::istream *_in;
   std::ostream *_out;
   std::vector<std::ostringstream*> _stack; // temporary output

   static const int TAB_WIDTH = 3;
   
protected:
   enum OutType { normal, beginl };

   std::ostream& out(OutType typ = normal);
   void indent(int x) { 
      _indent += x; 
      assert(_indent >= 0);
   }

   std::istream& in() { return *_in; }

   void push() { _stack.push_back(new std::ostringstream()); }

   std::string pop()  {
      std::string res = _stack.back()->str();
      delete _stack.back();
      _stack.pop_back();
      return res;
   }

public:
   ReadWriter(std::ostream *o)     : _indent(0),         _out(o) {}
   ReadWriter(std::istream *i = 0, 
              std::ostream *o = 0) : _indent(0), _in(i), _out(o) {}

   std::string indentation() const { 
      return std::string(_indent * TAB_WIDTH, ' '); 
   }
};

#endif