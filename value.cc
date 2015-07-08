#include <iostream>
#include <iomanip>
#include <sstream>
#include <map>
#include <typeinfo>
#include <assert.h>
using namespace std;

#include "value.hh"
#include "types.hh"

void *Value::abstract = (void*)0; // DANGER: this are not 'const'...
void *Value::unknown  = (void*)1;

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

Value::Value(const Type *t, void *d, bool cnst) {
   assert(t != 0);
   _attach(new Box(t, d));
   _const = cnst;
}

Value::Value(Box *box, bool cnst) { 
   _const = cnst;
   _attach(box); 
}

Value::~Value() {
   _detach(_box);
   _box = 0;
   _const = false;
}

Value::Value(const Value& v) {
   if (v.is_null()) {
      _box = 0;
   } else {
      _attach(v._box);
   }
   _const = v._const;
}

const Value& Value::operator=(const Value& v) {
   _detach(_box);
   if (v.is_null()) {
      _box = 0;
   } else {
      _attach(v._box);
   }
   _const = v._const;
   return *this;
}

bool Value::same_type_as(const Value& v) const {
   if (is_null()) {
      return v.is_null();
   }
   if (v.is_null()) {
      return is_null();
   }
   // If you compare the pointers, you are obliged to have singletons
   // for every type. But in the case of chars, for instance, there 
   // are two 'char' types, one for references (which are not destroyed)
   // and one for normal 'char' variables.
   return _box->type->typestr() == v._box->type->typestr(); 
}

Value Value::clone() const {
   if (is_null()) {
      return Value();
   }
   void *data = 0;
   if (_box->data != 0) {
      data = _box->type->clone(_box->data);
   }
   return Value(_box->type, data, _const);
}

bool Value::assign(const Value& v) {
   if (_const) {
      return false;
   }
   if (v.is_null()) {
      _detach(_box);
      _box = 0;
      return true;
   }
   if (!same_type_as(v)) {
      return false;
   }
   if (v.is_unknown() or v.is_abstract()) {
      _box->type->destroy(_box->data);
      _box->data = (v.is_unknown() ? unknown : abstract);
      _box->touched = true;
      return true;
   } 
   else if (is_abstract() or is_unknown()) {
      _box->data = v._box->type->clone(v._box->data);
      _box->touched = true;
      return true;
   }
   else {
      _box->touched = _box->type->assign(_box->data, v._box->data);
      return _box->touched;
   }
}

void Value::clear_touched() {
   if (_box != 0) {
      _box->touched = false;
      if (_box->data != 0) {
         _box->type->clear_touched(_box->data);
      }
   }
}

void Value::touch() {
   if (_box != 0) {
      _box->touched = true;
   }
}

void Value::write(ostream& o) const {
   assert(!is_null());
   _box->type->write(o, _box->data);
}

void Value::read(istream& i) {
   assert(!is_null());
   _box->data = _box->type->read(i, _box->data);
   _box->touched = true;
}

bool Value::equals(const Value& v) const {
   if (is_null()) {
      return v.is_null();
   }
   if (!same_type_as(v)) {
      return false;
   }
   if (_box->data == 0) {
      return v._box->data == 0;
   }
   if (v._box->data == 0) {
      return _box->data == 0;
   }
   return _box->type->equals(_box->data, v._box->data);
}

bool Value::less_than(const Value& v) const {
   assert(same_type_as(v));
   if (_box->data == 0 or v._box->data == 0) {
      return false;
   }
   return _box->type->less_than(_box->data, v._box->data);
}

string Value::type_name() const {
   assert(_box != 0);
   return _box->type->typestr();
}

string Value::to_json() const {
   ostringstream json;
   if (is_null()) {
      json << "null";
   } else {
      json << "{\"box\":\"" << (void*)_box << "\"";
      if (_box->touched) {
         json << ",\"<touched>\":true";
      }
      json << ",\"data\":";
      if (_box->data == 0) {
         json << "null";
      } else {
         json << _box->type->to_json(_box->data);
      }
      json << "}";
   }
   return json.str();
}

Value::Value(int x)         : _const(false) { _attach(new Box(Int::self,    Int::self->alloc(x))); }
Value::Value(char x)        : _const(false) { _attach(new Box(Char::self,   Char::self->alloc(x))); }
Value::Value(bool x)        : _const(false) { _attach(new Box(Bool::self,   Bool::self->alloc(x))); }
Value::Value(float x)       : _const(false) { _attach(new Box(Float::self,  Float::self->alloc(x))); }
Value::Value(double x)      : _const(false) { _attach(new Box(Double::self, Double::self->alloc(x))); }
Value::Value(string x)      : _const(false) { _attach(new Box(String::self, String::self->alloc(x))); }
Value::Value(ostream& o)    : _const(false) { _attach(new Box(OStream::self, &o)); }
Value::Value(istream& i)    : _const(false) { _attach(new Box(IStream::self, &i)); }
Value::Value(const char *x) : _const(false) { _attach(new Box(String::self, String::self->alloc(string(x)))); }

std::ostream& operator<<(std::ostream& o, const Value& v) {
   v.write(o);
   return o;
}

std::istream& operator>>(std::istream& i, Value& v) {
   v.read(i);
   return i;
}
