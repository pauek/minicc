#ifndef TYPES_HH
#define TYPES_HH

#include <vector>
#include "value.hh"

class Type {
   //      void *alloc(T x) = a different method for every Type
   virtual void  destroy(void *data) const = 0;

public:
   enum Properties {
      Basic       = 1, Emulated = 2, 
      UserDefined = 4, Template = 8
   };

   virtual         int properties() const = 0;
   virtual      Value_ create() const = 0;
   virtual      Value_ convert(Value_ init) const = 0;
   virtual      Value_ construct(const std::vector<Value_>& args) const = 0;
   virtual std::string str() const = 0;

   friend class Value_;
   friend class Reference;
};

template<typename T>
class BasicType : public Type {
   std::string _name;
public:
   typedef T cpp_type;

   BasicType(std::string name) : _name(name) {}
   int properties()  const { return Type::Basic; }
   std::string str() const { return _name; }

   Value_ create() const { return Value_(this); }

   Value_ construct(const std::vector<Value_>& args) const {
      assert(false); // Basic types are not "constructed"
      return 0;
   }

   void *alloc(T x)           const { return new T(x); }
   void  destroy(void *data)  const { delete static_cast<T*>(data); }

   static T cast(void *data) { return *static_cast<T*>(data); }
};

class Reference : public Type {
   Type *_subtype;
public:
   Reference(Type *subtype) : _subtype(subtype) {}

   std::string str() const { return _subtype->str() + "&"; }
   int    properties() const { return Basic; }

   void *alloc(Value_& x);
   void  destroy(void *data);
   
   Value_ create() const { assert(false); } // a reference must be initialized
   Value_ convert(Value_ init) const;
   Value_ construct(const std::vector<Value_>& args) const { assert(false); } // not an object
};

class Int : public BasicType<int> {
public:
   Int() : BasicType("int") {}
   Value_ convert(Value_ init) const;
   static Int *self;
};

class Float : public BasicType<float> {
public:
   Float() : BasicType("float") {}
   Value_ convert(Value_ init) const;
   static Float *self;
};

class Double : public BasicType<double> {
public:
   Double() : BasicType("double") {}
   Value_ convert(Value_ init) const;
   static Double *self;
};


class Char : public BasicType<char> {
public:
   Char() : BasicType("char") {}
   Value_ convert(Value_ init) const;
   static Char *self;
};

class Bool : public BasicType<bool> {
public:
   Bool() : BasicType("bool") {}
   Value_ convert(Value_ init) const;
   static Bool *self;
};

class String : public BasicType<std::string> {
public:
   String() : BasicType("string") {}
   Value_ convert(Value_ init) const;
   static String *self;
};

class Vector : public Type {
   Type *celltype;
   Value *_newvec(int sz) const;
public:
   Vector(Type *_celltype) : celltype(_celltype) {}

   int    properties() const { return Template | Emulated; }
   Value_ create() const;
   Value_ convert(Value_ init) const;
   Value_ construct(const std::vector<Value_>& args) const;
};

// Value_ template methods (DO NOT MOVE)

template<typename T>
bool Value_::is() const {
   assert(_box);
   return typeid(*(_box->type)) == typeid(T); 
}

template<typename T>
typename T::cpp_type Value_::as() const {
   assert(is<T>() and _box);
   return T::cast(_box->data);
}

#endif
