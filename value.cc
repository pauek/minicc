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

Value::Value(Type *t, void *d) {
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
   return _box->type->typestr();
}

string Value::to_json() const {
   ostringstream json;
   json << "{\"box\":\"" << (void*)_box << "\",\"data\":";
   json << (is_null() ? "\"?\"" : _box->type->to_json(_box->data));
   json << "}";
   return json.str();
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
