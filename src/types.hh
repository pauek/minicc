#ifndef TYPES_HH
#define TYPES_HH

#include <vector>
#include <map>
#include <set>
#include <sstream>
#include "ast.hh"
#include "value.hh"

using std::string;

struct TypeError : public Error {
   TypeError(std::string _msg) : Error(_msg) {}
};

class Interpreter;
struct Func {
   std::string name;
   Func(std::string n) : name(n) {}
   virtual ~Func() {}
   virtual Value call(Value self, const std::vector<Value>& args) = 0;
   virtual bool  call_abstract(Ast *x, Value self, const std::vector<Value>& args) { return false; }
};

class Type {
   std::string _name;
   static std::map<string, const Type *> reference_types;

public:
   Type() {}
   Type(std::string name) : _name(name) {}

   static const Type *mkref(const Type *t);
   
   enum Property {
      Unknown     =  1, Basic       =   2,  
      Emulated    =  4, UserDefined =   8,  
      Template    = 16, Internal    =  32,
      Class       = 64, Composite   = 128,
   };

   //             void *alloc(T x) = a different method for every Type
   virtual        void  destroy(void *data)                const { assert(false); }
   virtual        bool  equals(void *a, void *b)           const { assert(false); }
   virtual        bool  less_than(void *a, void *b)        const { assert(false); }
   virtual        bool  assign(void *a, void *b)           const { assert(false); }
   virtual        void *clone(void *data)                  const { assert(false); }
   virtual        void  write(std::ostream& o, void *data) const { assert(false); }
   virtual        void *read(std::istream& i, void *data)  const { assert(false); }
   virtual      string  ToJson(void *data)                const { assert(false); }
   virtual        void  clear_touched(void *data)          const { assert(false); }
   virtual        bool  contains_unknowns(void *data)      const { return false; }

   virtual std::string  name()                             const { return _name; }
   virtual std::string  TypeStr()                          const { return _name; }
   virtual         int  properties()                       const = 0;
   virtual         int  get_field(Value self,
                                  std::string, 
                                  std::vector<Value>& M)   const { return false; }
   virtual        bool  get_static(std::string, Value& v)  const { return false; }
   virtual        Type *get_inner_class(std::string)             { return 0; }
   virtual        Type *instantiate(std::vector<Type*>& s) const { assert(false); } // for templates
                                    //     subtypes ----^
   virtual       Value  create()                                 { assert(false); }
   virtual       Value  create_abstract()                  const { return Value(this, Value::abstract); }
   virtual       Value  convert(Value init)                const { assert(false); }
   virtual        bool  accepts(const Type *t)             const { return this == t; }

   template<typename T>    bool  is() const { return dynamic_cast<const T*>(this) != 0; }
   template<typename T> const T *as() const { return dynamic_cast<const T*>(this); }
   template<typename T>       T *as()       { return dynamic_cast<T*>(this); }

   bool is(Property prop) const { return properties() & prop; }

   friend class Value;
   friend class Reference;
};

extern Type *Void, *Any;

class Environment;
class TypeMap {
   std::map<std::string, Type*> _typemap;
   std::map<std::string, Type*> _typecache; // all types indexed by typestr

   Type *instantiate_template(const std::vector<TypeSpec*>& subtypespecs, 
                              Type *T, Environment *topmost);
public:
   void  register_type(std::string name, Type *);
   Type *get_type(TypeSpec *spec, Environment *topmost);
   void  clear();
};

class UnknownType : public Type {
public:
   UnknownType()            : Type("<unknown>") {}
   UnknownType(string name) : Type("<unknown:'" + name + "'>") {}

   int properties() const { return Type::Unknown; }

   Value create() { return Value(this, Value::unknown); }
   Value convert(Value v) const;

   void destroy(void *data) const {
      assert(data == Value::unknown or data == Value::abstract); // should only be used in abstract/unknown values.
   }
   
   static UnknownType *self;
};

template<typename T>
class BaseType : public Type {
public:
   BaseType(std::string name) : Type(name) {}

   int properties() const { return Internal; }

   typedef T cpp_type;
   static T& cast(void *data) { 
      return *static_cast<T*>(data); 
   }

   void *alloc(T x) const { 
      return new T(x); 
   }
   void destroy(void *data) const {
      if (data == Value::abstract or 
          data == Value::unknown) {
         return;
      }
      delete static_cast<T*>(data); 
   }
   bool equals(void *a, void *b) const {
      if (a == Value::unknown or a == Value::abstract or 
          b == Value::unknown or b == Value::abstract) {
         return false;
      }
      return (*static_cast<T*>(a)) == (*static_cast<T*>(b));
   }
   bool assign(void *a, void *b) const {
      *static_cast<T*>(a) = *static_cast<T*>(b);
      return true;
   }
   void *clone(void *data) const {
      if (data == Value::unknown or data == Value::abstract) {
         return data;
      }
      return new T(*static_cast<T*>(data));
   }
   Value create() { 
      return Value(this, Value::unknown); 
   }
   Value convert(Value init) const {
      if (init.has_type(this)) {
         return init.clone();
      }
      return Value::null;
   }
   void clear_touched(void *data) const {} // nothing to do
   bool accepts(const Type *t) const;
};

template<typename T>
class BasicType : public BaseType<T> {
   std::string ToJson(void *data) const {
      if (data == Value::unknown or data == Value::abstract) {
         return "null";
      }
      std::ostringstream o;
      o << *static_cast<const T*>(data);
      return o.str();
   }

public:
   BasicType(std::string name) : BaseType<T>(name) {}
   int properties()      const { return Type::Basic; }
   bool less_than(void *a, void *b) const {
      assert(a != Value::abstract and b != Value::abstract);
      return (*static_cast<T*>(a)) < (*static_cast<T*>(b));
   }
   void *read(std::istream& i, void *data) const {
      if (data == Value::unknown) {
         data = new T;
      }
      i >> (*static_cast<T*>(data));
      return data;
   }
   void write(std::ostream& o, void *data) const {
      if (data == Value::unknown) {
         o << "?";
      } else {
         o << (*static_cast<T*>(data));
      }
   }
};

class Reference : public Type {
   const Type *_subtype;
public:
   Reference(const Type *subtype) : Type("<reference>"), _subtype(subtype) {}

         Value  create_abstract()   const;

    const Type *subtype()           const { return _subtype; }
   std::string  TypeStr()           const { return _subtype->TypeStr() + "&"; }
           int  properties()        const { return Basic; }

          void *alloc(Value& x)     const;
          void  destroy(void *data) const;
   
         Value  convert(Value init) const;

  static Value  mkref(Value& v);  // create a reference to a value
  static Value  deref(const Value& v);  // obtain the referenced value

          void  clear_touched(void *data) const;

   std::string  ToJson(void *data) const;

   static Reference *self;
};

class Int : public BasicType<int> {
public:
   Int() : BasicType("int") {}
   Value convert(Value init)    const; 
    bool accepts(const Type *t) const;
  static Int *self;
};

class Float : public BasicType<float> {
public:
   Float() : BasicType("float") {}
   Value convert(Value init)    const;
    bool accepts(const Type *t) const;
   static Float *self;
};

class Double : public BasicType<double> {
public:
   Double() : BasicType("double") {}
   Value convert(Value init)    const;
    bool accepts(const Type *t) const;
   static Double *self;
};

class Char : public BasicType<char> {
   bool _destroy; // reference to a char within a string
public:
   Char(bool destroy = true) : _destroy(destroy), BasicType("char") {}
   Value convert(Value init)       const; 
    bool accepts(const Type *t)    const;
    void destroy(void *data)       const;
   std::string ToJson(void *data) const;
   static Char *self;
   static Char *self_ref;
};

/*

TODO: CharRef, un tipo que permite obtener una referencia a un
caracter individual de un string pero que cuando es modificado, marca
el "touched" del string entero. Esto hace que sea necesario tener
una referencia al string entero y un índice, y que no se pueda hacer
como la clase "Reference".

 */

class Bool : public BasicType<bool> {
public:
   Bool() : BasicType("bool") {}
   Value convert(Value init)    const;
    bool accepts(const Type *t) const; 
  static Bool *self;
   std::string ToJson(void *data) const {
      return (*(bool*)data ? "true" : "false");
   }
};

class Function;

template<class Base>
class Class : public Base {
   std::multimap<std::string, Value> _methods;
   std::map<std::string, Value> _statics;
   SimpleTable<Type*> _inner_classes;
protected:
   void _add_static(std::string, Value);
   void _add_method(Function *type, Func *f);
   void _add_inner_class(Type *type) {
      _inner_classes.set(type->name(), type);
   }
public:
   Class(std::string name) : Base(name) {}

   int properties() const { return Type::Class | Type::Composite; }

   bool  get_static(std::string name, Value& result) const;
    int  get_field(Value self, std::string name, std::vector<Value>& result) const;
   Type *get_inner_class(std::string name) {
      Type *t;
      return (_inner_classes.get(name, t) ? t : 0);
   }
};

class String : public Class<BasicType<std::string>> {
public:
   String();
   static String *self;
   int properties() const { return Internal | Class<BasicType<std::string>>::properties(); }
   std::string ToJson(void *data) const;
   Value create() { return Value((Type*)this, (void*)(new std::string())); }
};

struct FuncPtr {
   Func *ptr;
   FuncPtr() : ptr(0) {}
   FuncPtr(Func *_ptr) : ptr(_ptr) {}
   bool operator==(const FuncPtr& p) const { return ptr == p.ptr; }
   // bool operator< (const FuncPtr& p) const { return ptr < p.ptr;  } // nonsensical...
};

class Function : public BaseType<FuncPtr> {
   const Type *_return_type;
   std::vector<Type*> _param_types;
public:
   Function(const Type *t) : BaseType<FuncPtr>("<function>"), _return_type(t) {}
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

    int num_params()       const { return _param_types.size(); }
   Type *param(int i)      const { return (i < _param_types.size() ? _param_types[i] : 0); }
const Type *return_type()     const { return _return_type; }
   bool is_void()          const { return _return_type == 0; }
    int check_signature(const std::vector<Value>& args) const;

   std::string TypeStr() const;

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

   Binding() {}
   Binding(Value _self, Value _func) 
      : self(_self), func(_func) {}

   Value call(const std::vector<Value>& args) {
      return func.as<Function>().ptr->call(self, args);
   }
   bool call_abstract(Ast *x, const std::vector<Value>& args) {
      return func.as<Function>().ptr->call_abstract(x, self, args);
   }
   bool operator==(const Binding& x) const {
      return self == x.self and func == x.func;
   }
};

class Callable : public BaseType<Binding> {
public:
              Callable() : BaseType<Binding>("<callable>") {}

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
              Overloaded() : BaseType<OverloadedValue>("<unresolved-function>") {}
       Value  convert(Value init) const { assert(false); }
       Value  mkvalue(Value self, const std::vector<Value>& candidates);

   static  Overloaded *self;
   typedef OverloadedValue cpp_type;
};

class Struct : public BaseType<SimpleTable<Value>> {
   SimpleTable<Type*> _fields;
public:
   Struct(std::string name) : BaseType<SimpleTable<Value>>(name) {}
   void  add_field(std::string field_name, Type *t) { _fields.set(field_name, t); }
   bool  has_field(std::string field_name)    const { return _fields.exists(field_name); }
   bool  contains_unknowns(void *data) const;

    int  properties()              const { return Type::UserDefined | Type::Composite; }
  Value  create();
  Value  convert(Value init)       const;
  Value  create_abstract()         const;
   void *clone(void *data)         const;
   void  clear_touched(void *data) const;

   std::string ToJson(void *data) const;

   typedef SimpleTable<Value> cpp_type;
};

/*------  TODO: Arrays multidimensionales!!!  ------*/

class Array : public BaseType<std::vector<Value>> {
          Type *_celltype;
           int  _sz;

   static Type *_mkarray(Type *celltype,
                         std::vector<int>::const_iterator curr,
                         const std::vector<int>& sizes);
public:
                Array(Type *celltype, int sz) 
                   : BaseType<std::vector<Value>>("<array>"), _celltype(celltype), _sz(sz) {}
   static Type *mkarray(Type *celltype, const std::vector<int>& sizes); // use this as constructor for 2D and up...
           int  properties() const { return Basic | Composite; }
   std::string  TypeStr()    const { return _celltype->TypeStr() + "[]"; }
          Type *celltype()   const { return _celltype; }
         Value  create();
         Value  create_abstract()   const;
         Value  convert(Value init) const;
          void  clear_touched(void *data) const;
          bool  contains_unknowns(void *data) const;

   std::string  ToJson(void *) const;
};

class Vector : public Class<BaseType<std::vector<Value>>> {
   Type *_celltype; // celltype == 0 means it's the template
public:
   Vector()        : Class("vector"), _celltype(0) {}
   Vector(Type *t);

   int   properties() const { return Template | Emulated | Type::Class; }
   Value convert(Value init) const;
   Type *instantiate(std::vector<Type*>& args) const;
   Type *celltype() const { return _celltype; }

   Value create() { 
      return Value(this, new std::vector<Value>()); 
   }

   void  clear_touched(void *data) const;
   
   std::string TypeStr() const;
   std::string ToJson(void *data) const;

   static Vector *self;

   typedef std::vector<Value> cpp_type;
   typedef std::vector<Value>::iterator cpp_iterator;
   static Value elem_to_value(Vector *, const Value& v) { return v; }
};

class List : public Class<BaseType<std::list<Value>>> {
   Type *_celltype; // celltype == 0 means it's the template
public:
   List()        : Class("list"), _celltype(0) {}
   List(Type *t);

   Value create() { 
      return Value(this, new std::list<Value>()); 
   }

   void  clear_touched(void *data) const;

   int   properties() const { return Template | Emulated; }
   Value convert(Value init) const;
   Type *instantiate(std::vector<Type*>& args) const;
   Type *celltype() const { return _celltype; }

   std::string TypeStr() const;
   std::string ToJson(void *data) const;

   static List *self;

   typedef std::list<Value> cpp_type;
   typedef std::list<Value>::iterator cpp_iterator;
   static Value elem_to_value(List *, const Value& v) { return v; }
};

class Pair : public Class<BaseType<std::pair<Value, Value>>> {
   Type *_first, *_second; // (_first == 0 && _second == 0) means it's the template

   typedef Class<BaseType<std::pair<Value, Value>>> Base;
   
public:
   Pair() : Class("pair"), _first(0), _second(0) {}
   Pair(Type *_1, Type *_2);

   Value create() { 
      return Value(this, new std::pair<Value, Value>()); 
   }

   int   properties() const { return Template | Emulated; }
   Value convert(Value init) const;
   Type *instantiate(std::vector<Type*>& args) const;
   Type *first()  const { return _first; }
   Type *second() const { return _second; }

   bool less_than(void *a, void *b) const;
    int get_field(Value self, std::string name, std::vector<Value>& result) const;

   std::string TypeStr() const;
   std::string ToJson(void *data) const;

   static Pair *self;

   typedef std::pair<Value, Value> cpp_type;
};

class Map : public Class<BaseType<std::map<Value, Value>>> {
   Type *_pair_type;
   Type *_key, *_value; // (_first == 0 && _second == 0) means it's the template

   typedef Class<BaseType<std::map<Value, Value>>> Base;
   
public:
   Map() : Class("map"), _key(0), _value(0), _pair_type(0) {}
   Map(Type *k, Type *v);

   Value create() { 
      return Value(this, new std::map<Value, Value>()); 
   }

   int   properties() const { return Template | Emulated; }
   Type *instantiate(std::vector<Type*>& args) const;
   Type *key()      const { return _key; }
   Type *value()    const { return _value; }
   Type *celltype() const { return _pair_type; }

   std::string TypeStr() const;
   std::string ToJson(void *data) const;

   static Map *self;

   typedef std::map<Value, Value> cpp_type;
   typedef std::map<Value, Value>::iterator cpp_iterator;
   static Value elem_to_value(Map *map_type, const std::pair<Value, Value>& elem) { 
      return Value(map_type->_pair_type, (void*)(new std::pair<Value, Value>(elem)));
   }
};

template<class C> /* C == Container */
class Iterator : public Class<BaseType<typename C::cpp_iterator>> {
   C *_container_type;
public:
   Iterator(C *type);

   std::string TypeStr() const { return _container_type->TypeStr() + "::iterator"; }
   std::string ToJson(void *data) const;

   typedef typename C::cpp_iterator cpp_type;
};

template<class C>
class ForwardIterator : public Iterator<C> {
public:
   ForwardIterator(C *type);
};

template<class C>
class BidirectionalIterator : public ForwardIterator<C> {
public:
   BidirectionalIterator(C *type);
};

template<class C>
class RandomAccessIterator : public BidirectionalIterator<C> {
public:
   RandomAccessIterator(C *type);
};

class VectorValue : public BaseType<std::vector<Value>> {
public:
   VectorValue() : BaseType<std::vector<Value>>("<vector-value>") {}
   typedef std::vector<Value> cpp_type;
   Value create()           { return Value(this, new std::vector<Value>()); }
   static Value make() { return self->create(); }
   static VectorValue *self;
};

class OStream : public Class<Type> {
   void destroy(void *data)  const {}
protected:
   void _add_ostream_methods();
   OStream(std::string name) : Class<Type>(name) { _add_ostream_methods(); }
public:
   OStream() : Class<Type>("ostream") { _add_ostream_methods(); }
   int properties()      const { return Emulated; }
   static OStream *self;

   static std::ostream& cast(void *data) { 
      return *static_cast<std::ostream*>(data); 
   }
   typedef std::ostream cpp_type;
};

class IStream : public Class<Type> {
   void destroy(void *data)  const {}
protected:
   void _add_istream_methods();
   IStream(std::string name) : Class<Type>(name) { _add_istream_methods(); }
public:
   IStream() : Class<Type>("istream") { _add_istream_methods(); }
   int properties()      const { return Emulated; }
   static IStream *self;

   static std::istream& cast(void *data) { 
      return *static_cast<std::istream*>(data); 
   }
   typedef std::istream& cpp_type;
};

class IStringStream : public IStream {
public:
   IStringStream();
   static IStringStream *self;
   Value create() { return Value(this, new std::istringstream()); }

   static std::istringstream& cast(void *data) { 
      return *static_cast<std::istringstream*>(data); 
   }
   typedef std::istringstream& cpp_type;
};

class OStringStream : public OStream {
public:
   OStringStream();
   Value create() { return Value(this, new std::ostringstream()); }
   static OStringStream *self;

   static std::ostringstream& cast(void *data) { 
      return *static_cast<std::ostringstream*>(data); 
   }
   typedef std::ostringstream& cpp_type;
};

// Value template methods (DO NOT MOVE)

template<typename T>
bool Value::is() const {
   return !is_null() and dynamic_cast<const T*>(_box->type) != 0;
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
std::string  ToJson() const;

Environment *parent() { return _parent; }
Environment *pop();
       void  set_active(bool x);

       void  using_namespace(Environment *nmspc);
       void  register_type(std::string name, Type *);
       Type *get_type(TypeSpec *spec, Environment *topmost = 0);
   
       bool  get(std::string name, Value& res);

       void  set(std::string name, Value data, int flags = 0) {
          _tab.set(name, data, flags);
       }

       void clear_touched() {
          _tab.for_each([](Value &v) { v.clear_touched(); });
          if (_parent) {
             _parent->clear_touched();
          }
       }

       bool  has_flag(std::string name, Flag f) const {
          return _tab.has_flag(name, f);
       }
};

// to assist visitors that use the environment
// 
class WithEnvironment { 
   typedef std::map<std::string, Environment*> NamespaceMap;
   Environment  *_env;
   NamespaceMap  _namespaces;
   std::istream *_in;
   std::ostream *_out;
public:
   WithEnvironment() 
      : _env(0), _in(0), _out(0) {}

   WithEnvironment(std::istream *i, std::ostream *o) 
      : _env(0), _in(i), _out(o) {}

   void  pushenv(std::string name) {
      _env = new Environment(name, _env);
   }

   void  popenv() {
      _env = _env->pop();
      assert(_env != 0);
      actenv();
   }

   void  actenv() {
      _env->set_active(true);
   }

   bool  getenv(std::string id, Value& v);
   void  setenv(std::string id, Value v, int flags = 0);
   bool  has_flag(std::string id, Flag f) const;

   void  clear_touched() { _env->clear_touched(); }
   
   Type *get_type(TypeSpec *spec);
   void  register_type(std::string name, Type *);

   Environment *get_namespace(string name);
   bool  using_namespace(string name);
   bool  include_header_file(string name);
   
   void  prepare_global_environment();

   string env2json() const;
};

std::string json_encode(std::string s);

#endif
