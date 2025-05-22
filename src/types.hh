#ifndef TYPES_HH
#define TYPES_HH

#include <map>
#include <set>
#include <sstream>
#include <vector>
#include "ast.hh"
#include "error.hh"
#include "value.hh"

using std::string;

class Interpreter;

struct Func {
    std::string name;

    Func(std::string n) : name(n) {}

    virtual ~Func() {}

    virtual Value call(Value self, const std::vector<Value>& args) = 0;

    virtual bool call_abstract(AstNodeCore *x, Value self, const std::vector<Value>& args) {
        return false;
    }
};

class Type {
    std::string                           _name;
    static std::map<string, const Type *> reference_types;

   public:
    Type() {}

    Type(std::string name) : _name(name) {}

    static const Type *mkref(const Type *t);

    enum Property {
        Unknown = 1,
        Basic = 2,
        Emulated = 4,
        UserDefined = 8,
        Template = 16,
        Internal = 32,
        Class = 64,
        Composite = 128,
    };

    //             void *alloc(T x) = a different method for every Type
    virtual void destroy(void *data) const  { assert(false); }

    virtual bool equals(void *a, void *b) const { assert(false); return false; }

    virtual bool less_than(void *a, void *b) const { assert(false); return false; }

    virtual bool assign(void *a, void *b) const { assert(false); return false; }

    virtual void *clone(void *data) const { assert(false); return nullptr; }

    virtual void write(std::ostream& o, void *data) const { assert(false); }

    virtual void *read(std::istream& i, void *data) const { assert(false); return nullptr; }

    virtual string to_json(void *data) const { assert(false); return ""; }

    virtual void clear_touched(void *data) const { assert(false); }

    virtual bool contains_unknowns(void *data) const { return false; }

    virtual std::string name() const { return _name; }

    virtual std::string TypeStr() const { return _name; }

    virtual int         properties() const = 0;
    virtual const Type *get_inner_class(std::string name) const = 0;

    virtual int get_field(Value self, std::string, std::vector<Value>& M) const { return false; }

    virtual bool get_static(std::string, Value& v) const { return false; }

    virtual const Type *instantiate(std::vector<const Type *>& s) const {
        assert(false);
        return nullptr;
    }  // for templates

    //     subtypes ----^
    virtual Value create() const { assert(false); return Value(); }

    virtual Value create_abstract() const { return Value(this, Value::abstract); }

    virtual Value convert(Value init) const { assert(false); return Value(); }

    virtual bool accepts(const Type *t) const { return this == t; }

    template <typename T>
    bool is() const {
        return dynamic_cast<const T *>(this) != 0;
    }

    template <typename T>
    const T *as() const {
        return dynamic_cast<const T *>(this);
    }

    template <typename T>
    T *as() {
        return dynamic_cast<T *>(this);
    }

    bool is(Property prop) const { return properties() & prop; }
    friend class Value;
    friend class Reference;
};

extern const Type *Void, *Any;
class Environment;

class TypeMap {
    std::map<std::string, const Type *> _typemap;
    std::map<std::string, const Type *> _typecache;  // all types indexed by typestr

    const Type *instantiate_template(
        const std::vector<TypeSpec *>& subtypespecs,
        const Type                    *T,
        Environment                   *topmost
    );

   public:
    void        register_type(std::string name, const Type *);
    const Type *get_type(TypeSpec *spec, Environment *topmost);
    void        clear();
};

class UnknownType : public Type {
   public:
    UnknownType() : Type("<unknown>") {}

    UnknownType(string name) : Type("<unknown:'" + name + "'>") {}

    int properties() const { return Type::Unknown; }

    const Type *get_inner_class(std::string name) const { return 0; }

    Value create() const { return Value(this, Value::unknown); }

    Value convert(Value v) const;

    void destroy(void *data) const {
        assert(
            data == Value::unknown || data == Value::abstract
        );  // should only be used in abstract/unknown values.
    }

    static const UnknownType *self;
};

template <typename T>
class BaseType : public Type {
   public:
    BaseType(std::string name) : Type(name) {}

    int properties() const { return Internal; }

    const Type *get_inner_class(std::string name) const { return 0; }

    typedef T cpp_type;

    static T& cast(void *data) { return *static_cast<T *>(data); }

    void *alloc(T x) const { return new T(x); }

    void destroy(void *data) const {
        if (data == Value::abstract or data == Value::unknown) {
            return;
        }
        delete static_cast<T *>(data);
    }

    bool equals(void *a, void *b) const {
        if (a == Value::unknown or a == Value::abstract or b == Value::unknown or
            b == Value::abstract) {
            return false;
        }
        return (*static_cast<T *>(a)) == (*static_cast<T *>(b));
    }

    bool assign(void *a, void *b) const {
        *static_cast<T *>(a) = *static_cast<T *>(b);
        return true;
    }

    void *clone(void *data) const {
        if (data == Value::unknown or data == Value::abstract) {
            return data;
        }
        return new T(*static_cast<T *>(data));
    }

    Value create() const { return Value(this, Value::unknown); }

    Value convert(Value init) const {
        if (init.has_type(this)) {
            return init.clone();
        }
        return Value::null;
    }

    void clear_touched(void *data) const {}  // nothing to do

    bool accepts(const Type *t) const;
};

template <typename T>
class BasicType : public BaseType<T> {
    std::string to_json(void *data) const {
        if (data == Value::unknown or data == Value::abstract) {
            return "null";
        }
        std::ostringstream o;
        o << *static_cast<const T *>(data);
        return o.str();
    }

   public:
    BasicType(std::string name) : BaseType<T>(name) {}

    int properties() const { return Type::Basic; }

    bool less_than(void *a, void *b) const {
        assert(a != Value::abstract and b != Value::abstract);
        return (*static_cast<T *>(a)) < (*static_cast<T *>(b));
    }

    void *read(std::istream& i, void *data) const {
        if (data == Value::unknown) {
            data = new T;
        }
        i >> (*static_cast<T *>(data));
        return data;
    }

    void write(std::ostream& o, void *data) const {
        if (data == Value::unknown) {
            o << "?";
        } else {
            o << (*static_cast<T *>(data));
        }
    }
};

class Reference : public Type {
    const Type *_subtype;

   public:
    Reference(const Type *subtype) : Type("<reference>"), _subtype(subtype) {}

    Value create_abstract() const;

    const Type *subtype() const { return _subtype; }

    std::string TypeStr() const { return _subtype->TypeStr() + "&"; }

    int properties() const { return Basic; }

    const Type *get_inner_class(std::string name) const { return 0; }

    void        *alloc(Value& x) const;
    void         destroy(void *data) const;
    Value        convert(Value init) const;
    static Value mkref(Value& v);        // create a reference to a value
    static Value deref(const Value& v);  // obtain the referenced value
    void         clear_touched(void *data) const;
    std::string  to_json(void *data) const;

    static const Reference *self;
};

class Int : public BasicType<int> {
   public:
    Int() : BasicType("int") {}

    Value convert(Value init) const;
    bool  accepts(const Type *t) const;

    static const Int *self;
};

class Float : public BasicType<float> {
   public:
    Float() : BasicType("float") {}

    Value convert(Value init) const;
    bool  accepts(const Type *t) const;

    static const Float *self;
};

class Double : public BasicType<double> {
   public:
    Double() : BasicType("double") {}

    Value convert(Value init) const;
    bool  accepts(const Type *t) const;

    static const Double *self;
};

class Char : public BasicType<char> {
    bool _destroy;  // reference to a char within a string
   public:
    Char(bool destroy = true) : _destroy(destroy), BasicType("char") {}

    Value       convert(Value init) const;
    bool        accepts(const Type *t) const;
    void        destroy(void *data) const;
    std::string to_json(void *data) const;

    static const Char *self;
    static const Char *self_ref;
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

    Value convert(Value init) const;
    bool  accepts(const Type *t) const;

    static const Bool *self;

    std::string to_json(void *data) const { return (*(bool *)data ? "true" : "false"); }
};
class Function;

template <class Base>
class Class : public Base {
    std::multimap<std::string, Value> _methods;
    std::map<std::string, Value>      _statics;
    Table<const Type *>               _inner_classes;

   protected:
    void _add_static(std::string, Value);
    void _add_method(const Function *type, Func *f);

    void _add_inner_class(Type *type) { _inner_classes.set(type->name(), type); }

   public:
    Class(std::string name) : Base(name) {}

    int properties() const { return Type::Class | Type::Composite; }

    bool get_static(std::string name, Value& result) const;
    int  get_field(Value self, std::string name, std::vector<Value>& result) const;

    const Type *get_inner_class(std::string name) const {
        const Type *t;
        return (_inner_classes.get(name, t) ? t : 0);
    }
};

class String : public Class<BasicType<std::string>> {
   public:
    String();
    static const String *self;

    int properties() const { return Internal | Class<BasicType<std::string>>::properties(); }

    std::string to_json(void *data) const;

    Value create() const { return Value((Type *)this, (void *)(new std::string())); }
};

struct FuncPtr {
    Func *ptr;

    FuncPtr() : ptr(0) {}

    FuncPtr(Func *_ptr) : ptr(_ptr) {}

    bool operator==(const FuncPtr& p) const { return ptr == p.ptr; }

    // bool operator< (const FuncPtr& p) const { return ptr < p.ptr;  } // nonsensical...
};

class Function : public BaseType<FuncPtr> {
    const Type               *_return_type;
    std::vector<const Type *> _param_types;

   public:
    Function(const Type *t) : BaseType<FuncPtr>("<function>"), _return_type(t) {}

    Function *add_params(const Type *t) {
        _param_types.push_back(t);
        return this;
    }

    Function *add_params(const Type *t1, const Type *t2) {
        _param_types.push_back(t1);
        _param_types.push_back(t2);
        return this;
    }

    Function *add_params(const Type *t1, const Type *t2, const Type *t3) {
        _param_types.push_back(t1);
        _param_types.push_back(t2);
        _param_types.push_back(t3);
        return this;
    }

    int num_params() const { return _param_types.size(); }

    const Type *param(int i) const { return (i < _param_types.size() ? _param_types[i] : 0); }

    const Type *return_type() const { return _return_type; }

    bool is_void() const { return _return_type == 0; }

    int         check_signature(const std::vector<Value>& args) const;
    std::string TypeStr() const;

    Value mkvalue(Func *f) const {
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

    Binding(Value _self, Value _func) : self(_self), func(_func) {}

    Value call(const std::vector<Value>& args) { return func.as<Function>().ptr->call(self, args); }

    bool call_abstract(AstNodeCore *x, const std::vector<Value>& args) {
        return func.as<Function>().ptr->call_abstract(x, self, args);
    }

    bool operator==(const Binding& x) const { return self == x.self and func == x.func; }
};

class Callable : public BaseType<Binding> {
   public:
    Callable() : BaseType<Binding>("<callable>") {}

    Value mkvalue(Value self, Value func) const { return Value(this, new Binding(self, func)); }

    static const Callable *self;
    typedef Binding        cpp_type;
};

struct OverloadedValue {
    Value              _self;
    std::vector<Value> _candidates;

    bool operator==(const OverloadedValue& v) const { return _candidates == v._candidates; }

    Value resolve(const std::vector<Value>& args);
};

class Overloaded : public BaseType<OverloadedValue> {
   public:
    Overloaded() : BaseType<OverloadedValue>("<unresolved-function>") {}

    Value convert(Value init) const { assert(false); return Value(); }

    Value mkvalue(Value self, const std::vector<Value>& candidates) const;

    static const Overloaded *self;
    typedef OverloadedValue  cpp_type;
};

class Struct : public BaseType<Table<Value>> {
    Table<const Type *> _fields;

   public:
    Struct(std::string name) : BaseType<Table<Value>>(name) {}

    void add_field(std::string field_name, const Type *t) { _fields.set(field_name, t); }

    bool has_field(std::string field_name) const { return _fields.exists(field_name); }

    bool contains_unknowns(void *data) const;

    int properties() const { return Type::UserDefined | Type::Composite; }

    Value       create() const;
    Value       convert(Value init) const;
    Value       create_abstract() const;
    void       *clone(void *data) const;
    void        clear_touched(void *data) const;
    std::string to_json(void *data) const;

    typedef Table<Value> cpp_type;
};

/*------  TODO: Arrays multidimensionales!!!  ------*/
class Array : public BaseType<std::vector<Value>> {
    const Type        *_celltype;
    int                _sz;
    static const Type *_mkarray(
        const Type                      *celltype,
        std::vector<int>::const_iterator curr,
        const std::vector<int>&          sizes
    );

   public:
    Array(const Type *celltype, int sz)
        : BaseType<std::vector<Value>>("<array>"), _celltype(celltype), _sz(sz) {}

    const Type *get_inner_class(std::string name) const { return 0; }

    static const Type *mkarray(
        const Type             *celltype,
        const std::vector<int>& sizes
    );  // use this as constructor for 2D and up...

    int properties() const { return Basic | Composite; }

    std::string TypeStr() const { return _celltype->TypeStr() + "[]"; }

    const Type *celltype() const { return _celltype; }

    Value       create() const;
    Value       create_abstract() const;
    Value       convert(Value init) const;
    void        clear_touched(void *data) const;
    bool        contains_unknowns(void *data) const;
    std::string to_json(void *) const;
};

class Vector : public Class<BaseType<std::vector<Value>>> {
    const Type *_celltype;  // celltype == 0 means it's the template
   public:
    Vector() : Class("vector"), _celltype(0) {}

    Vector(const Type *t);

    int properties() const { return Template | Emulated | Type::Class; }

    Value       convert(Value init) const;
    const Type *instantiate(std::vector<const Type *>& args) const;

    const Type *celltype() const { return _celltype; }

    Value create() const { return Value(this, new std::vector<Value>()); }

    void        clear_touched(void *data) const;
    std::string TypeStr() const;
    std::string to_json(void *data) const;

    static const Vector *self;

    typedef std::vector<Value>           cpp_type;
    typedef std::vector<Value>::iterator cpp_iterator;

    static Value elem_to_value(const Vector *, const Value& v) { return v; }
};

class List : public Class<BaseType<std::list<Value>>> {
    const Type *_celltype;  // celltype == 0 means it's the template
   public:
    List() : Class("list"), _celltype(0) {}

    List(const Type *t);

    Value create() const { return Value(this, new std::list<Value>()); }

    void clear_touched(void *data) const;

    int properties() const { return Template | Emulated; }

    Value       convert(Value init) const;
    const Type *instantiate(std::vector<const Type *>& args) const;

    const Type *celltype() const { return _celltype; }

    std::string TypeStr() const;
    std::string to_json(void *data) const;

    static const List *self;

    typedef std::list<Value>           cpp_type;
    typedef std::list<Value>::iterator cpp_iterator;

    static Value elem_to_value(const List *, const Value& v) { return v; }
};

class Pair : public Class<BaseType<std::pair<Value, Value>>> {
    const Type *_first, *_second;  // (_first == 0 && _second == 0) means it's the template
    typedef Class<BaseType<std::pair<Value, Value>>> Base;

   public:
    Pair() : Class("pair"), _first(0), _second(0) {}

    Pair(const Type *_1, const Type *_2);

    Value create() const { return Value(this, new std::pair<Value, Value>()); }

    int properties() const { return Template | Emulated; }

    Value       convert(Value init) const;
    const Type *instantiate(std::vector<const Type *>& args) const;

    const Type *first() const { return _first; }

    const Type *second() const { return _second; }

    bool        less_than(void *a, void *b) const;
    int         get_field(Value self, std::string name, std::vector<Value>& result) const;
    std::string TypeStr() const;
    std::string to_json(void *data) const;

    static const Pair *self;

    typedef std::pair<Value, Value> cpp_type;
};

class Map : public Class<BaseType<std::map<Value, Value>>> {
    const Type *_pair_type;
    const Type *_key, *_value;  // (_first == 0 && _second == 0) means it's the template
    typedef Class<BaseType<std::map<Value, Value>>> Base;

   public:
    Map() : Class("map"), _key(0), _value(0), _pair_type(0) {}

    Map(const Type *k, const Type *v);

    Value create() const { return Value(this, new std::map<Value, Value>()); }

    int properties() const { return Template | Emulated; }

    const Type *instantiate(std::vector<const Type *>& args) const;

    const Type *key() const { return _key; }

    const Type *value() const { return _value; }

    const Type *celltype() const { return _pair_type; }

    std::string TypeStr() const;
    std::string to_json(void *data) const;

    static const Map *self;

    typedef std::map<Value, Value>           cpp_type;
    typedef std::map<Value, Value>::iterator cpp_iterator;

    static Value elem_to_value(const Map *map_type, const std::pair<Value, Value>& elem) {
        return Value(map_type->_pair_type, (void *)(new std::pair<Value, Value>(elem)));
    }
};

template <class C> /* C == Container */
class Iterator : public Class<BaseType<typename C::cpp_iterator>> {
    const C *_container_type;

   public:
    Iterator(const C *type);

    std::string TypeStr() const { return _container_type->TypeStr() + "::iterator"; }

    std::string to_json(void *data) const;

    typedef typename C::cpp_iterator cpp_type;
};

template <class C>
class ForwardIterator : public Iterator<C> {
   public:
    ForwardIterator(const C *type);
};

template <class C>
class BidirectionalIterator : public ForwardIterator<C> {
   public:
    BidirectionalIterator(const C *type);
};

template <class C>
class RandomAccessIterator : public BidirectionalIterator<C> {
   public:
    RandomAccessIterator(const C *type);
};

class VectorValue : public BaseType<std::vector<Value>> {
   public:
    VectorValue() : BaseType<std::vector<Value>>("<vector-value>") {}

    const Type *get_inner_class(std::string name) const { return 0; }

    typedef std::vector<Value> cpp_type;

    Value create() const { return Value(this, new std::vector<Value>()); }

    static Value make() { return self->create(); }

    static const VectorValue *self;
};

class OStream : public Class<Type> {
    void destroy(void *data) const {}

   protected:
    void _add_ostream_methods();

    OStream(std::string name) : Class<Type>(name) { _add_ostream_methods(); }

   public:
    OStream() : Class<Type>("ostream") { _add_ostream_methods(); }

    int properties() const { return Emulated; }

    static const OStream *self;

    static std::ostream& cast(void *data) { return *static_cast<std::ostream *>(data); }

    typedef std::ostream cpp_type;
};

class IStream : public Class<Type> {
    void destroy(void *data) const {}

   protected:
    void _add_istream_methods();

    IStream(std::string name) : Class<Type>(name) { _add_istream_methods(); }

   public:
    IStream() : Class<Type>("istream") { _add_istream_methods(); }

    int properties() const { return Emulated; }

    static const IStream *self;

    static std::istream& cast(void *data) { return *static_cast<std::istream *>(data); }

    typedef std::istream& cpp_type;
};

class IStringStream : public IStream {
   public:
    IStringStream();
    static const IStringStream *self;

    Value create() const { return Value(this, new std::istringstream()); }

    static std::istringstream& cast(void *data) { return *static_cast<std::istringstream *>(data); }

    typedef std::istringstream& cpp_type;
};

class OStringStream : public OStream {
   public:
    OStringStream();

    Value create() const { return Value(this, new std::ostringstream()); }

    static const OStringStream *self;

    static std::ostringstream& cast(void *data) { return *static_cast<std::ostringstream *>(data); }

    typedef std::ostringstream& cpp_type;
};

// Value template methods (DO NOT MOVE)
template <typename T>
bool Value::is() const {
    return !is_null() and dynamic_cast<const T *>(_box->type) != 0;
}

template <typename T>
typename T::cpp_type& Value::as() {
    assert(is<T>() and _box);
    return T::cast(_box->data);
}

template <typename T>
typename T::cpp_type& Value::as() const {
    assert(is<T>() and _box);
    return T::cast(_box->data);
}

class Environment {
    std::string             _name;
    bool                    _hidden;
    Environment            *_parent;
    Table<Value>            _tab;
    bool                    _active;
    TypeMap                 _curr_namespace;
    std::set<Environment *> _other_namespaces;

   public:
    Environment(std::string name, Environment *parent, bool hidden = false)
        : _name(name), _parent(parent), _hidden(hidden), _active(false) {}

    bool hidden() const { return _hidden; }

    std::string to_json() const;

    Environment *parent() { return _parent; }

    Environment *pop();
    void         set_active(bool x);
    void         using_namespace(Environment *nmspc);
    void         register_type(std::string name, const Type *);
    const Type  *get_type(TypeSpec *spec, Environment *topmost = 0);
    bool         get(std::string name, Value& res);

    void set(std::string name, Value data, int flags = 0) { _tab.set(name, data, flags); }

    void clear_touched() {
        _tab.for_each([](Value& v) { v.clear_touched(); });
        if (_parent) {
            _parent->clear_touched();
        }
    }

    bool has_flag(std::string name, Flag f) const { return _tab.has_flag(name, f); }
};

// to assist visitors that use the environment
//
class WithEnvironment {
    typedef std::map<std::string, Environment *> NamespaceMap;

    Environment  *_env;
    NamespaceMap  _namespaces;
    std::istream *_in;
    std::ostream *_out;

   public:
    WithEnvironment() : _env(0), _in(0), _out(0) {}

    WithEnvironment(std::istream *i, std::ostream *o) : _env(0), _in(i), _out(o) {}

    void pushenv(std::string name) { _env = new Environment(name, _env); }

    void popenv() {
        _env = _env->pop();
        assert(_env != 0);
        actenv();
    }

    void actenv() { _env->set_active(true); }

    bool getenv(std::string id, Value& v);
    void setenv(std::string id, Value v, int flags = 0);
    bool has_flag(std::string id, Flag f) const;

    void clear_touched() { _env->clear_touched(); }

    const Type  *get_type(TypeSpec *spec);
    void         register_type(std::string name, const Type *);
    Environment *get_namespace(string name);
    bool         using_namespace(string name);
    bool         include_header_file(string name);
    void         prepare_global_environment();
    string       env2json() const;
};

std::string json_encode(std::string s);

#endif
