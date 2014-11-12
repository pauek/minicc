
#include <vector>
#include <sstream>
using namespace std;

#include "types.hh"

Int    *Int::self    = new Int();
Float  *Float::self  = new Float();
Double *Double::self = new Double();
Char   *Char::self   = new Char();
Bool   *Bool::self   = new Bool();
String *String::self = new String();

void *Reference::alloc(Value_& x) {
   Value_::Box *b = x._box;
   b->count++;
   return b;
}

void Reference::destroy(void *data) {
   Value_::Box *b = (Value_::Box*)data;
   if (--(b->count) == 0) {
      b->type->destroy(b->data);
      delete b;
   }
}

Value_ Reference::convert(Value_ init) const {
   assert(init.type() == _subtype);
   return Value_();
}

// Initializations
Value_ Int::convert(Value_ x) const {
   if (x.is<Int>()) {
      return x;
   } else if (x.is<Float>()) {
      return Value_(int(x.as<Float>()));
   } else if (x.is<Double>()) {
      return Value_(int(x.as<Double>()));
   } else if (x.is<Char>()) {
      return Value_(int(x.as<Char>()));
   } else if (x.is<Bool>()) {
      return Value_(int(x.as<Bool>()));
   }
   assert(false);
}

Value_ Float::convert(Value_ x) const {
   if (x.is<Float>()) {
      return x;
   } else if (x.is<Int>()) {
      return Value_(float(x.as<Int>()));
   } else if (x.is<Double>()) {
      return Value_(float(x.as<Double>()));
   }
   assert(false);
}

Value_ Double::convert(Value_ x) const {
   if (x.is<Double>()) {
      return x;
   } else if (x.is<Int>()) {
      return Value_(float(x.as<Int>()));
   } else if (x.is<Float>()) {
      return Value_(float(x.as<Float>()));
   }
   assert(false);
}

Value_ Char::convert(Value_ x) const {
   if (x.is<Char>()) {
      return x;
   } else if (x.is<Int>()) {
      return Value_(char(x.as<Int>()));
   }
   assert(false);
}

Value_ Bool::convert(Value_ x) const {
   if (x.is<Bool>()) {
      return x;
   } else if (x.is<Int>()) {
      return Value_(x.as<Int>() > 0);
   }
   assert(false);
}

Value_ String::convert(Value_ x) const {
   if (x.is<String>()) {
      return x;
   } 
   assert(false);
}

Value_ Vector::create() const {
   return Value_(this, (void*)new vector<Value_>());
}

Value_ Vector::convert(Value_ init) const {
   if (init.is<Vector>()) {
      return init;
   }
   assert(false);
}

Value_ Vector::construct(const vector<Value_>& args) const {
   assert(args.size() >= 0 and args.size() <= 2);
   if (args.size() == 0) {
      return create();
   }
   assert(args[0].is<Int>());
   const int sz = args[0].as<Int>();
   vector<Value_> *vec = new vector<Value_>(sz);
   for (int i = 0; i < sz; i++) {
      (*vec)[i] = (args.size() == 2 
                   ? celltype->convert(args[1]) 
                   : celltype->create());
   }
   return Value_(this, vec);
}

