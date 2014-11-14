#include <iostream>
#include <iomanip>
#include <sstream>
#include <map>
#include <typeinfo>
#include <assert.h>
using namespace std;

#include "value.hh"
#include "types.hh"

Value Value::null;

void Value::_attach(Box *b) {
   assert(b != 0);
   _box = b;
   (_box->count)++;
}

void Value::_detach(Box *b) {
   if (b and --(b->count) == 0) {
      b->type->destroy(b->data);
      delete b;
   }
}

Value::Value(const Type *t, void *d) {
   assert(t != 0);
   _attach(new Box(t, d));
}

Value::Value(Box *box) { 
   _attach(box); 
}

Value::~Value() {
   _detach(_box);
}

Value::Value(const Value& v) {
   if (v.is_null()) {
      _box = 0;
   } else {
      _attach(v._box);
   }
}

const Value& Value::operator=(const Value& v) {
   _detach(_box);
   if (v.is_null()) {
      _box = 0;
   } else {
      _attach(v._box);
   }
   return *this;
}

Value Value::clone() const {
   if (is_null()) {
      return Value();
   }
   return Value(_box->type, _box->type->clone(_box->data));
}

bool Value::assign(const Value& v) {
   if (v.is_null()) {
      _detach(_box);
      _box = 0;
      return true;
   }
   if (!same_type_as(v)) {
      return false;
   }
   _box->type->destroy(_box->data);
   _box->data = _box->type->clone(v._box->data);
   return true;
}

void Value::write(ostream& o) const {
   assert(!is_null());
   _box->type->write(o, _box->data);
}

void Value::read(istream& i) {
   assert(!is_null());
   _box->data = _box->type->read(i, _box->data);
}

bool Value::equals(const Value& v) const {
   if (is_null()) {
      return v.is_null();
   }
   if (!same_type_as(v)) {
      return false;
   }
   return _box->type->equals(_box->data, v._box->data);
}

string Value::type_name() const {
   assert(_box != 0);
   return _box->type->name();
}

string Value::to_json() const {
   assert(!is_null());
   return _box->type->to_json(_box->data);
}

Value::Value(int x)         { _attach(new Box(Int::self,    Int::self->alloc(x))); }
Value::Value(char x)        { _attach(new Box(Char::self,   Char::self->alloc(x))); }
Value::Value(bool x)        { _attach(new Box(Bool::self,   Bool::self->alloc(x))); }
Value::Value(float x)       { _attach(new Box(Float::self,  Float::self->alloc(x))); }
Value::Value(double x)      { _attach(new Box(Double::self, Double::self->alloc(x))); }
Value::Value(string x)      { _attach(new Box(String::self, String::self->alloc(x))); }
Value::Value(ostream& o)    { _attach(new Box(Ostream::self, &o)); }
Value::Value(istream& i)    { _attach(new Box(Istream::self, &i)); }
Value::Value(const char *x) { _attach(new Box(String::self, String::self->alloc(string(x)))); }

std::ostream& operator<<(std::ostream& o, const Value& v) {
   v.write(o);
   return o;
}

std::istream& operator>>(std::istream& i, Value& v) {
   v.read(i);
   return i;
}


string Environment::to_json() const {
   ostringstream json;
   json << "{\"<active>\":" << (active ? "true" : "false");
   for (int i = 0; i < tab.size(); i++) {
      if (tab[i]._hidden) {
         continue;
      }
      json << ",\"" << tab[i].name() << "\":";
      json << tab[i].data().to_json();
   }
   json << "}";
   return json.str();
}


/*

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
   case Value::ExprList: {
      const vector<Value*> *vals = v.exprlist();
      o << "{";
      for (int i = 0; i < vals->size(); i++) {
         if (i > 0) {
            o << ", ";
         }
         o << *(*vals)[i];
      }
      o << '}';
      return o;
   }
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

Value::Kind Value::type2kind(string type) {
   if (type == "int") {
      return Value::Int;
   } else if (type == "char") {
      return Value::Char;
   } else if (type == "float") {
      return Value::Float;
   } else if (type == "double") {
      return Value::Double;
   } else if (type == "bool") {
      return Value::Bool;
   } else if (type == "string") {
      return Value::String;
   } else if (type.substr(0,6) == "struct") {
      return Value::Struct;
   } else if (type.substr(0,6) == "vector") {
      return Value::Vector;
   } else if (type.size() >= 2 and type.substr(type.size()-2, 2) == "[]") {
      return Value::Array;
   } else {
      return Value::Unknown;
   }
}
*/

/*
   ostringstream json;
   switch (kind) {
   case Value::Unknown: json << "null"; break;
   case Value::Bool:    json << (val.as_bool ? "true" : "false"); break;
   case Value::Int:     json << val.as_int;                       break;
   case Value::Float:   json << val.as_float;                     break;
   case Value::Double:  json << val.as_double;                    break;
   case Value::Char: {
      if (val.as_char < 32 or val.as_char > 127) {
         json << "\"char(" << int(val.as_char) << ")\"";
      } else {
         json << "\"'" << val.as_char << "'\"";
      }
      break;
   }
   case Value::String: {
      string *s = static_cast<string*>(val.as_ptr);
      json << "\"\\\"" << json_encode(*s) << "\\\"\"";
      break;
   }
   case Value::Array: {
      vector<Value*> *v = static_cast<vector<Value*>*>(val.as_ptr);
      json << "[";
      for (int i = 0; i < v->size(); i++) {
         if (i > 0) {
            json << ", ";
         }
         json << (*v)[i]->to_json();
      }
      json << "]";
      break;
   }
   case Value::Struct: {
      Environment *E = static_cast<Environment*>(val.as_ptr);
      E->to_json(json);
      break;
   }
   case Value::Ref:
      json << "{\"ref\":\"" << static_cast<Value*>(val.as_ptr) << "\"}";
      break;
   default:
      json << "\"to_json(Value): UNIMPLEMENTED\""; 
   }
   return json.str();
}
*/


/*
vector<Value*> *Value::exprlist() {
   return static_cast<vector<Value*>*>(val.as_ptr);
}

const vector<Value*> *Value::exprlist() const {
   return static_cast<vector<Value*>*>(val.as_ptr);
}

*/
