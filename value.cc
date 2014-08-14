
#include <iostream>
using namespace std;

#include "value.hh"

ostream& operator<<(ostream& o, const Value& v) {
   switch (v.kind) {
   case Value::Bool:   o << v.val.as_bool;   break;
   case Value::Char:   o << v.val.as_char;   break;
   case Value::Int:    o << v.val.as_int;    break;
   case Value::Float:  o << v.val.as_float;  break;
   case Value::Double: o << v.val.as_double; break;
   case Value::String: {
      string *s = static_cast<string*>(v.val.as_ptr);
      o << *s;
      break;
   }
   default:
      o << "operator<<(Value): UNIMPLEMENTED"; 
      break;
   }
   return o;
}
