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
   Type *reference_type;

   //      void *alloc(T x) = a different method for every Type
   virtual void   destroy(void *data) const                 { assert(false); }
   virtual bool   equals(void *data_a, void *data_b)  const { assert(false); }
   virtual void  *clone(void *data)                   const { assert(false); }
   virtual void   write(std::ostream& o, void *data)  const { assert(false); }
   virtual void  *read(std::istream& i, void *data)   const { assert(false); }
   virtual string to_json(void *data)                 const { assert(false); }

public:
   Type() : reference_type(0) {}

   static Type *mkref(Type *t);
   
   enum Property {
      Basic       = 1,  Emulated = 2, 
      UserDefined = 4,  Template = 8,
      Internal    = 16
   };

   typedef Value (*Method)(void *, const std::vector<Value>& args);

   virtual std::string  typestr() const = 0;
   virtual         int  properties() const = 0;
   virtual        bool  get_method(std::string, std::pair<Type*, Method>&) const { return false; }
   virtual       Value  create()                                           { assert(false); }
   virtual       Value  convert(Value init)                                { assert(false); }
   virtual       Value  construct(const std::vector<Value>& args)          { assert(false); }
   virtual        Type *instantiate(std::vector<Type*>& subtypes)    const { assert(false); } // for templates

   template<typename T>
   bool is() const { return dynamic_cast<const T*>(this) != 0; }

   bool is(Property prop) const { return properties() | prop; }

   template<typename T>
   const T *as() const { return dynamic_cast<const T*>(this); }

   friend class Value;
   friend class Reference;
};

class TypeMap {
   std::map<std::string, Type*> _global_namespace;
   std::map<std::string, Type*> _typecache; // all types indexed by typestr
public:
   void  register_type(std::string name, Type *);
   Type *get_type(TypeSpec *);
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
   Value create() { 
      return Value(this, 0); 
   }
   Value convert(Value init) {
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
   BasicType(std::string name) : _name(name) {}
   int properties()      const { return Type::Basic; }
   std::string typestr() const { return _name; }

   Value construct(const std::vector<Value>& args) {
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

   std::string  typestr()           const { return _subtype->typestr() + "&"; }
           int  properties()        const { return Basic; }

          void *alloc(Value& x)     const;
          void  destroy(void *data) const;
   
         Value  convert(Value init);

  static Value  mkref(Value& v);  // create a reference to a value
  static Value  deref(const Value& v);  // obtain the referenced value

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
   Value convert(Value init);
   static Int *self;
};

class Float : public BasicType<float> {
public:
   Float() : BasicType("float") {}
   Value convert(Value init);
   static Float *self;
};

class Double : public BasicType<double> {
public:
   Double() : BasicType("double") {}
   Value convert(Value init);
   static Double *self;
};

class Char : public BasicType<char> {
public:
   Char() : BasicType("char") {}
   Value convert(Value init);
   static Char *self;
};

class Bool : public BasicType<bool> {
public:
   Bool() : BasicType("bool") {}
   Value convert(Value init);
   static Bool *self;
};

class String : public BasicType<std::string> {
public:
   String() : BasicType("string") {}
   static String *self;
   std::string to_json(void *data) {
      return string("\"") + *(string*)data + "\"";
   }

   bool get_method(std::string name, std::pair<Type*, Method>& method) const;
private:
   static std::map<
      std::string, 
      std::pair<std::function<Type *()>, Method>
   > _methods;
};


typedef Value (*CppFunc)(const std::vector<Value>& args);
typedef Value (*CppMethod)(void *, const std::vector<Value>& args);

class Interpreter;
struct FuncPtr {
   virtual void invoke(Interpreter* I, const std::vector<Value>& args) = 0;
   virtual ~FuncPtr() {}
};

struct FuncValue {
   std::string name;
   FuncPtr *ptr;
   FuncValue(std::string n, FuncPtr *p) : name(n), ptr(p) {}
   void invoke(Interpreter *I, const std::vector<Value>& args);

   bool operator==(const FuncValue& f) const { return ptr == f.ptr; }
};

class Function : public BaseType<FuncValue> {
   Type *_return_type;
   std::vector<Type*> _param_types;
public:
   Function(Type *t) : _return_type(t) {}
   Function *add_param(Type *t)  { _param_types.push_back(t); return this; }
   Function *add_params(Type *t1, Type *t2)  { 
      _param_types.push_back(t1);
      _param_types.push_back(t2);
      return this; 
   }
   Function *add_params(Type *t1, Type *t2, Type *t3)  { 
      _param_types.push_back(t1);
      _param_types.push_back(t2);
      _param_types.push_back(t3);
      return this; 
   }

   Type *param(int i)      const { return _param_types[i]; }
   Type *return_type()     const { return _return_type; }
   bool is_void()          const { return _return_type == 0; }

   int properties() const { return Internal; }
   std::string typestr() const;

   Value mkvalue(std::string name, FuncPtr *pf) {
      return Value(this, new FuncValue(name, pf));
   }

   typedef FuncValue cpp_type;
};

class Struct : public BaseType<SimpleTable<Value>> {
   std::string        _name;
   SimpleTable<Type*> _fields;
public:
   Struct(std::string name) : _name(name) {}
   void add_field(std::string field_name, Type *t) { _fields.set(field_name, t); }

   int   properties() const { return Internal; }
   Value create();
   Value convert(Value init);
   void *clone(void *data) const;

   std::string typestr() const { return _name; }
   std::string to_json(void *data) const {
      assert(false);
   }

   typedef SimpleTable<Value> cpp_type;
};

class Array : public BaseType<std::vector<Value>> {
   Type *_celltype;
   int _sz;
public:
                Array(Type *celltype, int sz) : _celltype(celltype), _sz(sz) {}
           int  properties() const { return Basic; }
   std::string  typestr()    const { return _celltype->typestr() + "[]"; }
         Value  create();
         Value  convert(Value init);
};

class Vector : public BaseType<std::vector<Value>> {
   Type *_celltype; // celltype == 0 means it's the template
public:
   Vector()        : _celltype(0) {}
   Vector(Type *t) : _celltype(t) {}

   Type *instantiate(std::vector<Type*>& args) const;
   
   typedef std::vector<Value> cpp_type;

   Type *celltype() const { return _celltype; }

   int   properties() const { return Template | Emulated; }
   Value create()           { return Value(this, (void*)(new std::vector<Value>())); }
   Value convert(Value init);
   Value construct(const std::vector<Value>& args);

   std::string typestr() const;
   bool get_method(std::string name, std::pair<Type*, Method>& method) const;

   std::string to_json(void *data) const;

   static Vector *self;

private:
   static std::map<
      std::string, 
      std::pair<std::function<Type *(Type *)>, Method>
   > _methods;
};

class VectorValue : public BaseType<std::vector<Value>> {
public:
   typedef std::vector<Value> cpp_type;
   int   properties() const { return Internal; }
   Value create()           { return Value(this, new std::vector<Value>()); }
   static Value make() { return self->create(); }
   static VectorValue *self;
   std::string typestr() const { return "vector<?>"; }
};

class Ostream : public Type {
   void destroy(void *data)  const {}
public:
   int properties()       const { return Emulated; }
   std::string typestr()  const { return "ostream"; }
   Value create()               { assert(false); }
   Value convert(Value init)    { assert(false); }
   Value construct(const std::vector<Value>& args) { assert(false); }

   static Ostream *self;
};

class Istream : public Type {
   void destroy(void *data)  const {}
public:
   int properties()          const { return Emulated; }
   Value create()            const { assert(false); }
   Value convert(Value init)       { assert(false); }
   Value construct(const std::vector<Value>& args) const { assert(false); }
   std::string typestr()     const { return "istream"; }

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
