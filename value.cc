
#include <iostream>
using namespace std;

#include "value.hh"

Value Value::cout(Cout), Value::cin(Cin), Value::cerr(Cerr);

Value::~Value() {
   switch (kind) {
   case String: 
      delete static_cast<string*>(val.as_ptr);
      break;

   default:
   case Array: case Vector: case List: case Map:  case Struct: 
      // TODO
      break;
   }
}

Value::Value(const Value& v) {
   _clear();
   kind = v.kind;
   type = v.type;
   switch (v.kind) {
   case String:
      val.as_ptr = new string(*static_cast<string*>(v.val.as_ptr));
      break;
   default:
      memcpy(&val, &v.val, sizeof(Value::Any));
      break;
   }
}

Value Value::operator=(const Value& v) {
   kind = v.kind;
   type = v.type;
   switch (v.kind) {
   case String:
      delete static_cast<string*>(val.as_ptr);
      val.as_ptr = new string(*static_cast<string*>(v.val.as_ptr));
      break;
   default:
      memcpy(&val, &v.val, sizeof(Value::Any));
      break;
   }
   return *this;
}

bool operator==(const Value& a, const Value& b) {
   if (a.kind != b.kind || a.type != b.type) {
      return false;
   }
   switch (a.kind) {
   case Value::Cout: case Value::Cerr: case Value::Cin: 
      return true;
   default:
      return memcmp(&a.val, &b.val, sizeof(Value::Any)) == 0;
   }
}

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
