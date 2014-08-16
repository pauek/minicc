
#include <iostream>
#include <assert.h>
using namespace std;

#include "value.hh"

Value Value::cout(Cout), Value::cin(Cin), Value::cerr(Cerr);

Value::~Value() {
   switch (kind) {
   case String: 
      delete static_cast<string*>(val.as_ptr);
      break;

   default:
   case Array: case Vector: case List: case Map: case Struct: 
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
   if (kind == Value::String) {
      delete static_cast<string*>(val.as_ptr);
   }
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
   return *this;
}

bool operator==(const Value& a, const Value& b) {
   if (a.kind != b.kind || a.type != b.type) {
      return false;
   }
   switch (a.kind) {
   case Value::Cout: case Value::Cerr: case Value::Cin: 
      return true;
   case Value::String: {
      string *sa = static_cast<string*>(a.val.as_ptr);
      string *sb = static_cast<string*>(b.val.as_ptr);
      return *sa == *sb;
   }
   default:
      return memcmp(&a.val, &b.val, sizeof(Value::Any)) == 0;
   }
}

ostream& operator<<(ostream& o, const Value& v) {
   switch (v.kind) {
   case Value::Bool:   return o << v.val.as_bool;
   case Value::Char:   return o << v.val.as_char;
   case Value::Int:    return o << v.val.as_int;
   case Value::Float:  return o << v.val.as_float;
   case Value::Double: return o << v.val.as_double;
   case Value::String: {
      string *s = static_cast<string*>(v.val.as_ptr);
      return o << *s;
   }
   case Value::Ref:
      return o << *static_cast<Value*>(v.val.as_ptr);
   default:
      return o << "operator<<(Value): UNIMPLEMENTED"; 
   }
   return o;
}

istream& operator>>(istream& i, Value& v) {
   if (v.type == "bool") {
      if (i >> v.val.as_bool) {
         v.kind = Value::Bool;
      }
   } else if (v.type == "char") {
      if (i >> v.val.as_char) {
         v.kind = Value::Char;
      }
   } else if (v.type == "int") {
      if (i >> v.val.as_int) {
         v.kind = Value::Int;
      }
   } else if (v.type == "float") {
      if (i >> v.val.as_float) {
         v.kind = Value::Float;
      }
   } else if (v.type == "double") {
      if (i >> v.val.as_double) {
         v.kind = Value::Double;
      }
   } else if (v.type == "string") {
      string s;
      if (i >> s) {
         v.val.as_ptr = new string(s);
         v.kind = Value::String;
      }
   } else {
      assert(false);
   }
   return i;
}
