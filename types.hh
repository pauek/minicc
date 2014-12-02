#ifndef TYPES_HH
#define TYPES_HH

#include <vector>
#include <map>
#include <set>
#include <sstream>
#include "ast.hh"
#include "value.hh"

using std::string;

struct TypeError {
   std::string msg;
   TypeError(std::string _msg) : msg(_msg) {}
};

struct Func {
   std::string name;
   Func(std::string n) : name(n) {}
   virtual ~Func() {}
   virtual Value call(Value self, const std::vector<Value>& args) = 0;
};

class Type {
   Type *reference_type;

public:
   //      void  *alloc(T x) = a different method for every Type
   virtual   void  destroy(void *data) const                 { assert(false); }
   virtual   bool  equals(void *data_a, void *data_b)  const { assert(false); }
   virtual   void *clone(void *data)                   const { assert(false); }
   virtual   void  write(std::ostream& o, void *data)  const { assert(false); }
   virtual   void *read(std::istream& i, void *data)   const { assert(false); }
   virtual string  to_json(void *data)                 const { assert(false); }

   Type() : reference_type(0) {}

   static Type *mkref(Type *t);
   
   enum Property {
      Basic       = 1,  Emulated = 2, 
      UserDefined = 4,  Template = 8,
      Internal    = 16
   };


   virtual std::string  typestr() const = 0;
   virtual         int  properties() const = 0;
   virtual        bool  get_method(std::string, 
                                   std::vector<Value>& M)         const { return false; }
   virtual       Value  create()                                        { assert(false); }
   virtual        bool  accepts(const Type *t)                    const { return this == t; }
   virtual       Value  convert(Value init)                             { assert(false); }
   virtual       Value  construct(const std::vector<Value>& args)       { assert(false); }
   virtual        Type *instantiate(std::vector<Type*>& subtypes) const { assert(false); } // for templates

   template<typename T>
   bool is() const { return dynamic_cast<const T*>(this) != 0; }

   bool is(Property prop) const { return properties() | prop; }

   template<typename T>
   const T *as() const { return dynamic_cast<const T*>(this); }

   template<typename T>
   T *as() { return dynamic_cast<T*>(this); }

   friend class Value;
   friend class Reference;
};

class Environment;
class TypeMap {
   std::map<std::string, Type*> _typemap;
   std::map<std::string, Type*> _typecache; // all types indexed by typestr
public:
   void  register_type(std::string name, Type *);
   Type *get_type(TypeSpec *, Environment *);
   void  clear();
};

template<typename T>
class BaseType : public Type {
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

   std::string to_json(void *data) const {
      if (data == 0) {
         return "\"?\"";
      }
      std::ostringstream o;
      o << *static_cast<const T*>(data);
      return o.str();
   }

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

    const Type *subtype()           const { return _subtype; }
   std::string  typestr()           const { return _subtype->typestr() + "&"; }
           int  properties()        const { return Basic; }

          void *alloc(Value& x)     const;
          void  destroy(void *data) const;
   
         Value  convert(Value init);

  static Value  mkref(Value& v);  // create a reference to a value
  static Value  deref(const Value& v);  // obtain the referenced value

   std::string to_json(void *data) const;

   static Reference *self;
};

class Int : public BasicType<int> {
public:
   Int() : BasicType("int") {}
   Value convert(Value init); 
    bool accepts(const Type *t) const;
  static Int *self;
};

class Float : public BasicType<float> {
public:
   Float() : BasicType("float") {}
   Value convert(Value init);
    bool accepts(const Type *t) const;
   static Float *self;
};

class Double : public BasicType<double> {
public:
   Double() : BasicType("double") {}
   Value convert(Value init);
    bool accepts(const Type *t) const;
   static Double *self;
};

class Char : public BasicType<char> {
public:
   Char() : BasicType("char") {}
   Value convert(Value init); 
    bool accepts(const Type *t) const;
  std::string to_json(void *data) const;
   static Char *self;
};

class Bool : public BasicType<bool> {
public:
   Bool() : BasicType("bool") {}
   Value convert(Value init);
    bool accepts(const Type *t) const; 
  static Bool *self;
   std::string to_json(void *data) const {
      return (*(bool*)data ? "true" : "false");
   }
};

class Function;

class String : public BasicType<std::string> {
   std::multimap<std::string, Value> _methods;
   void _add_method(Function *type, Func *f);

public:
   String();
   static String *self;
   std::string to_json(void *data) const;

   bool get_method(std::string name, std::vector<Value>& result) const;
};

struct FuncPtr {
   Func *ptr;
   FuncPtr(Func *_ptr) : ptr(_ptr) {}
   bool operator==(const FuncPtr& p) const { return ptr == p.ptr; }
};

class Function : public BaseType<FuncPtr> {
   Type *_return_type;
   std::vector<Type*> _param_types;
public:
   Function(Type *t) : _return_type(t) {}
   Function *add_params(Type *t)  { _param_types.push_back(t); return this; }
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
   bool check_signature(const std::vector<Value>& args) const;

   int properties() const { return Internal; }
   std::string typestr() const;

   Value mkvalue(Func *f) { 
      // FIXME: Too many boxes, I should be able to call
      // Value(this, f). But this changes Function and I guess
      // cannot derive from BaseType<T> anymore...
      return Value(this, new FuncPtr(f)); 
   }

   typedef FuncPtr cpp_type;
};

struct Binding {
   Value self;
   Value func;

   Binding(Value _self, Value _func) 
      : self(_self), func(_func) {}

   Value call(const std::vector<Value>& args) {
      return func.as<Function>().ptr->call(self, args);
   }
   bool operator==(const Binding& x) const {
      return self == x.self and func == x.func;
   }
};

class Callable : public BaseType<Binding> {
public:
              Callable() {}
         int  properties()  const { return Internal; }
 std::string  typestr()     const { return "<binding>"; }

   Value  mkvalue(Value self, Value func) {
      return Value(this, new Binding(self, func));
   }

   static  Callable *self;
   typedef Binding cpp_type;
};

struct OverloadedValue {
   Value _self;
   std::vector<Value> _candidates;
   
   bool operator==(const OverloadedValue& v) const {
      return _candidates == v._candidates;
   }

   Value resolve(const std::vector<Value>& args);
};

class Overloaded : public BaseType<OverloadedValue> {
public:
              Overloaded() {}
         int  properties()  const { return Internal; }
 std::string  typestr()     const { return "<unresolved-function>"; }
       Value  convert(Value init) { assert(false); }
       Value  mkvalue(Value self, const std::vector<Value>& candidates);

   static  Overloaded *self;
   typedef OverloadedValue cpp_type;
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

   std::string to_json(void *data) const;

   typedef SimpleTable<Value> cpp_type;
};

class Array : public BaseType<std::vector<Value>> {
          Type *_celltype;
           int  _sz;

   static Type *_mkarray(Type *celltype,
                         std::vector<int>::const_iterator curr,
                         const std::vector<int>& sizes);
public:
                Array(Type *celltype, int sz) : _celltype(celltype), _sz(sz) {}
   static Type *mkarray(Type *celltype, const std::vector<int>& sizes); // use this as constructor for 2D and up...
           int  properties() const { return Basic; }
   std::string  typestr()    const { return _celltype->typestr() + "[]"; }
         Value  create();
         Value  convert(Value init);
   std::string  to_json(void *) const;
};

class Vector : public BaseType<std::vector<Value>> {
   Type *_celltype; // celltype == 0 means it's the template
   std::multimap<std::string, Value> _methods;
   void _add_method(Function *type, Func *f);

public:
   Vector()        : _celltype(0) {}
   Vector(Type *t);

   Type *instantiate(std::vector<Type*>& args) const;
   
   typedef std::vector<Value> cpp_type;

   Type *celltype() const { return _celltype; }

   int   properties() const { return Template | Emulated; }
   Value create()           { return Value(this, (void*)(new std::vector<Value>())); }
   Value convert(Value init);
   Value construct(const std::vector<Value>& args);

   std::string typestr() const;
   bool get_method(std::string name, std::vector<Value>& result) const;

   std::string to_json(void *data) const;

   static Vector *self;
   static Value default_value_for(Type *t);
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
   static Ostream *self;
};

class Istream : public Type {
   void destroy(void *data)  const {}
public:
   int properties()      const { return Emulated; }
   std::string typestr() const { return "istream"; }
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
typename T::cpp_type& Value::as() {
   assert(is<T>() and _box);
   return T::cast(_box->data);
}

template<typename T>
typename T::cpp_type& Value::as() const {
   assert(is<T>() and _box);
   return T::cast(_box->data);
}

class Environment {
             std::string  _name;
                    bool  _hidden;
             Environment *_parent;
      SimpleTable<Value>  _tab;
                    bool  _active;
                 TypeMap  _curr_namespace;
  std::set<Environment*>  _other_namespaces;

public:
   Environment(std::string name, Environment *parent, bool hidden = false) 
      : _name(name), _parent(parent), _hidden(hidden), _active(false) {}

       bool  hidden() const { return _hidden; }
std::string  to_json() const;

Environment *parent() { return _parent; }
Environment *pop();
       void  set_active(bool x);

       void  using_namespace(Environment *nmspc);
       void  register_type(std::string name, Type *);
       Type *get_type(TypeSpec *);
   
       bool  get(std::string name, Value& res);
       void  set(std::string name, Value data, bool hidden = false);
};

std::string json_encode(std::string s);

#endif
