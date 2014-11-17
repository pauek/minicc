#ifndef TYPES_HH
#define TYPES_HH

#include <vector>
#include <map>
#include <sstream>
#include "ast.hh"
#include "value.hh"

using std::string;

struct TypeError {
   std::string msg;
   TypeError(std::string _msg) : msg(_msg) {}
};

class Type {
   //      void *alloc(T x) = a different method for every Type
   virtual void   destroy(void *data) const                 { assert(false); }
   virtual bool   equals(void *data_a, void *data_b)  const { assert(false); }
   virtual void  *clone(void *data) const                   { assert(false); }
   virtual void   write(std::ostream& o, void *data)  const { assert(false); }
   virtual void  *read(std::istream& i, void *data)   const { assert(false); }
   virtual string to_json(void *data)                 const { assert(false); }

public:
   enum Properties {
      Basic       = 1,  Emulated = 2, 
      UserDefined = 4,  Template = 8,
      Internal    = 16
   };

   virtual        int properties() const = 0;
   virtual      Value create() const { assert(false); }
   virtual      Value convert(Value init) const { assert(false); }
   virtual      Value construct(const std::vector<Value>& args) const { assert(false); }
   virtual std::string name() const = 0;

   template<typename T>
   bool is() const { return dynamic_cast<const T*>(this) != 0; }

   template<typename T>
   const T *as() const { return dynamic_cast<const T*>(this); }

   friend class Value;
   friend class Reference;

   // Type registry   
   static void register_type(Type *);
   static Type *find(TypeSpec *);
   static Type *find(std::string);
private:
   static std::map<std::string, Type*> _typemap;
};

template<typename T>
class BaseType : public Type {
   std::string to_json(void *data) {
      std::ostringstream o;
      o << *static_cast<T*>(data);
      return o.str();
   }

public:
   typedef T cpp_type;
   static T& cast(void *data) { 
      return *static_cast<T*>(data); 
   }

   void *alloc(T x) const { 
      return new T(x); 
   }
   void destroy(void *data) const { 
      if (data == 0) {
         return;
      }
      delete static_cast<T*>(data); 
   }
   bool equals(void *a, void *b) const {
      if (a == 0 or b == 0) {
         return false;
      }
      return (*static_cast<T*>(a)) == (*static_cast<T*>(b));
   }
   void *clone(void *data) const {
      if (data == 0) {
         return 0;
      }
      return new T(*static_cast<T*>(data));
   }
   Value create() const { 
      return Value(this, 0); 
   }
   Value convert(Value init) const {
      if (init.has_type(this)) {
         return init.clone();
      }
      return Value::null;
   }
};

template<typename T>
class BasicType : public BaseType<T> {
   std::string _name;
public:
   BasicType(std::string name) : _name(name) { Type::register_type(this); }
   int properties()  const { return Type::Basic; }
   std::string name() const { return _name; }

   Value construct(const std::vector<Value>& args) const {
      assert(false); // Basic types are not "constructed"
      return Value::null;
   }
   void *read(std::istream& i, void *data) const {
      if (data == 0) {
         data = new T;
      }
      i >> (*static_cast<T*>(data));
      return data;
   }
   void write(std::ostream& o, void *data) const {
      if (data == 0) {
         o << "?";
      } else {
         o << (*static_cast<T*>(data));
      }
   }
};

class Reference : public Type {
   const Type *_subtype;
public:
   Reference(const Type *subtype) : _subtype(subtype) {}

   std::string name() const { return _subtype->name() + "&"; }
   int    properties() const { return Basic; }

   void *alloc(Value& x) const;
   void  destroy(void *data) const;
   
   Value convert(Value init) const;

   static Value mkref(Value& v);  // create a reference to a value
   static Value deref(const Value& v);  // obtain the referenced value

   std::string to_json(void *data) {
      Value::Box *b = (Value::Box*)data;
      std::ostringstream O;
      O << b->data;
      return O.str();
   }

   static Reference *self;
};

class Int : public BasicType<int> {
public:
   Int() : BasicType("int") {}
   Value convert(Value init) const;
   static Int *self;
};

class Float : public BasicType<float> {
public:
   Float() : BasicType("float") {}
   Value convert(Value init) const;
   static Float *self;
};

class Double : public BasicType<double> {
public:
   Double() : BasicType("double") {}
   Value convert(Value init) const;
   static Double *self;
};

class Char : public BasicType<char> {
public:
   Char() : BasicType("char") {}
   Value convert(Value init) const;
   static Char *self;
};

class Bool : public BasicType<bool> {
public:
   Bool() : BasicType("bool") {}
   Value convert(Value init) const;
   static Bool *self;
};

class String : public BasicType<std::string> {
public:
   String() : BasicType("string") {}
   static String *self;
   std::string to_json(void *data) {
      return string("\"") + *(string*)data + "\"";
   }
};

class Vector : public BaseType<std::vector<Value>> {
 protected:
   Type *_celltype;
 public:
   Vector(Type *celltype) : _celltype(celltype) {}
   
   typedef std::vector<Value> cpp_type;

   Type *celltype() const { return _celltype; }

   int   properties() const { return Template | Emulated; }
   Value create()     const { return Value(this, (void*)(new std::vector<Value>())); }
   Value convert(Value init) const;
   Value construct(const std::vector<Value>& args) const;

   std::string name() const { return std::string("vector<") + _celltype->name() + ">"; }

   std::string to_json(void *data) const;
};

typedef Value (*CppFunc)(const std::vector<Value>& args);
typedef Value (*CppMethod)(void *, const std::vector<Value>& args);

struct FuncPtr {
   virtual void invoke(const std::vector<Value>& args) = 0;
   virtual ~FuncPtr() {}
};

struct FuncValue {
   std::string name;
   FuncPtr *ptr;
   FuncValue(std::string n, FuncPtr *p) : name(n), ptr(p) {}
   void invoke(const std::vector<Value>& args) { ptr->invoke(args); }

   bool operator==(const FuncValue& f) const { return ptr == f.ptr; }
};

class Function : public BaseType<FuncValue> {
   Type *_return_type;
   std::vector<Type*> _param_types;
public:
   Function(Type *t) : _return_type(t) {}
   void add_param(Type *t)       { _param_types.push_back(t); }
   Type *return_type()     const { return _return_type; }
   bool is_void()          const { return _return_type == 0; }

   int properties() const { return Internal; }
   std::string name() const;

   Value mkvalue(std::string name, FuncPtr *pf) const {
      return Value(this, new FuncValue(name, pf));
   }

   typedef FuncValue cpp_type;
};

class Struct : public BaseType<SimpleTable<Value>> {
   std::string        _name;
   SimpleTable<Type*> _fields;
public:
   Struct(std::string name) : _name(name) {}
   void add(std::string field_name, Type *t) { _fields.set(field_name, t); }

   int   properties() const { return Internal; }
   Value create()     const;
   void *clone(void *data) const;
   Value convert(Value init) const;

   std::string name() const { return _name; }
   std::string to_json(void *data) const {
      assert(false);
   }

   typedef SimpleTable<Value> cpp_type;
};

class Array : public Vector {
   int _sz;
public:
   Array(Type *celltype, int sz) : _sz(sz), Vector(celltype) {}
   int properties() const { return Basic; }
   std::string name() const { return celltype()->name() + "[]"; }
   Value create() const;
   Value convert(Value init) const;
   static Type *mktype(Type *celltype, int sz);
};

class VectorValue : public BaseType<std::vector<Value>> {
public:
   typedef std::vector<Value> cpp_type;
   int   properties() const { return Internal; }
   Value create() const { return Value(this, new std::vector<Value>()); }
   static Value make() { return self->create(); }
   static VectorValue *self;
   std::string name() const { return "vector<?>"; }
};

class Ostream : public Type {
   void destroy(void *data)  const {}
public:
   int properties()          const { return Emulated; }
   Value create()            const { assert(false); }
   Value convert(Value init) const { assert(false); }
   Value construct(const std::vector<Value>& args) const { assert(false); }
   std::string name()         const { return "ostream"; }

   static Ostream *self;
};

class Istream : public Type {
   void destroy(void *data)  const {}
public:
   int properties()          const{ return Emulated; }
   Value create()            const { assert(false); }
   Value convert(Value init) const { assert(false); }
   Value construct(const std::vector<Value>& args) const { assert(false); }
   std::string name()        const { return "istream"; }

   static Istream *self;

   typedef std::istream& cpp_type;
   static std::istream& cast(void *data) { 
      return *static_cast<std::istream*>(data); 
   }
};

// Value template methods (DO NOT MOVE)

template<typename T>
bool Value::is() const {
   return !is_null() and (typeid(*(_box->type)) == typeid(T)); 
}

template<typename T>
typename T::cpp_type& Value::as() const {
   assert(is<T>() and _box);
   return T::cast(_box->data);
}

#endif
