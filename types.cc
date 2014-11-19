
#include <vector>
#include <sstream>
using namespace std;

#include "types.hh"

void _error(std::string msg) {
   throw TypeError(msg);
}

// static + Globals

Int         *Int::self         = new Int();
Float       *Float::self       = new Float();
Double      *Double::self      = new Double();
Char        *Char::self        = new Char();
Bool        *Bool::self        = new Bool();
String      *String::self      = new String();
Ostream     *Ostream::self     = new Ostream();
Istream     *Istream::self     = new Istream();
VectorValue *VectorValue::self = new VectorValue();
Vector      *Vector::self      = new Vector();

Value Cout(cout), Cerr(cerr);
Value Cin(cin);
Value Endl("\n");

// Methods

Type *TypeMap::get_type(TypeSpec *spec) {
   // 1. If typestr already registered, return the type
   {
      auto it = _typecache.find(spec->typestr());
      if (it != _typecache.end()) {
         return it->second;
      }
   }
   // 2. Construct the Type from the TypeSpec
   {
      auto it = _typemap.find(spec->id->name);
      if (it == _typemap.end()) {
         return 0;
      }
      Type *T = it->second;
      if (spec->is_template()) {
         assert(T->is(Type::Template));
         vector<Type*> subtypes;
         for (int i = 0; i < spec->id->subtypes.size(); i++) {
            subtypes.push_back(get_type(spec->id->subtypes[i]));
         }
         T = T->instantiate(subtypes);
      }
      if (spec->reference) {
         T = new Reference(T);
      }
      _typecache[spec->typestr()] = T;
      return T;
   }
}

void TypeMap::register_type(string name, Type *typespec) {
   assert(_typemap.find(name) == _typemap.end());
   _typemap[name] = typespec;
   _typecache[typespec->typestr()] = typespec;
}

void TypeMap::clear() {
   _typemap.clear();
   _typecache.clear();
}

Type *Type::mkref(Type *t) {
   if (t->reference_type == 0) {
      t->reference_type = new Reference(t);
   }
   return t->reference_type;
}

void *Reference::alloc(Value& x) const {
   Value::Box *b = x._box;
   b->count++;
   return b;
}

void Reference::destroy(void *data) const {
   Value::Box *b = (Value::Box*)data;
   if (--(b->count) == 0) {
      b->type->destroy(b->data);
      delete b;
   }
}

Value Reference::convert(Value init) {
   assert(init.type() == _subtype);
   return Value();
}

Value Reference::mkref(Value& v) {
   Value::Box *b = v._box;
   v._box->count++;
   return Value(Type::mkref(v._box->type), (void*)b);
}

Value Reference::deref(const Value& v) {
   if (v.is<Reference>()) {
      Value::Box *b = (Value::Box*)v._box->data;
      return Value(b);
   } else {
      return v;
   }
}

// Initializations
Value Int::convert(Value x) {
   if (x.is<Int>()) {
      return x.clone();
   } else if (x.is<Float>()) {
      return Value(int(x.as<Float>()));
   } else if (x.is<Double>()) {
      return Value(int(x.as<Double>()));
   } else if (x.is<Char>()) {
      return Value(int(x.as<Char>()));
   } else if (x.is<Bool>()) {
      return Value(int(x.as<Bool>()));
   }
   return Value::null;
}

Value Float::convert(Value x) {
   if (x.is<Float>()) {
      return x.clone();
   } else if (x.is<Int>()) {
      return Value(float(x.as<Int>()));
   } else if (x.is<Double>()) {
      return Value(float(x.as<Double>()));
   }
   return Value::null;
}

Value Double::convert(Value x) {
   if (x.is<Double>()) {
      return x.clone();
   } else if (x.is<Int>()) {
      return Value(float(x.as<Int>()));
   } else if (x.is<Float>()) {
      return Value(float(x.as<Float>()));
   }
   return Value::null;
}

Value Char::convert(Value x) {
   if (x.is<Char>()) {
      return x.clone();
   } else if (x.is<Int>()) {
      return Value(char(x.as<Int>()));
   }
   return Value::null;
}

Value Bool::convert(Value x) {
   if (x.is<Bool>()) {
      return x.clone();
   } else if (x.is<Int>()) {
      return Value(x.as<Int>() > 0);
   }
   return Value::null;
}

Value Vector::convert(Value init) {
   //
   // To support C++11-style initialization of vectors, this method should
   // be like Array::convert...
   //
   if (init.is<Vector>()) {
      return init.clone();
   }
   return Value::null;
}

Value Vector::construct(const vector<Value>& args) {
   assert(args.size() >= 0 and args.size() <= 2);
   if (args.size() == 0) {
      return create();
   }
   assert(args[0].is<Int>());
   Value arg0 = Reference::deref(args[0]);
   const int sz = arg0.as<Int>();
   vector<Value> *vec = new vector<Value>(sz);

   Value init;
   if (args.size() == 2) { // initialization
      init = Reference::deref(args[1]);
   } else {
      // Valor por defecto para cada tipo controlado por vector!
      if (_celltype->is<Int>()) {
         init = Value(0);
      } else if (_celltype->is<Bool>()) {
         init = Value(false);
      } else if (_celltype->is<Float>()) {
         init = Value(0.0f);
      } else if (_celltype->is<Double>()) {
         init = Value(0.0);
      } else if (_celltype->is<Char>()) {
         init = Value('\0');
      } else {
         init = _celltype->create();
      }
   }
   for (int i = 0; i < sz; i++) {
      (*vec)[i] = init.clone();
   }
   return Value(this, vec);
}

string Vector::to_json(void *data) const {
   ostringstream o;
   o << "[";
   vector<Value>& vec = *(vector<Value>*)data;
   for (int i = 0; i < vec.size(); i++) {
      if (i > 0) {
         o << ", ";
      }
      o << vec[i].to_json();
   }
   o << "]";
   return o.str();
}

Value Array::create() {
   vector<Value> *array = new vector<Value>(_sz);
   for (int i = 0; i < _sz; i++) {
      (*array)[i] = _celltype->create();
   }
   return Value(this, array);
}

Value Array::convert(Value init) {
   assert(!init.is_null());
   if (!init.is<VectorValue>()) {
      _error("Inicializas una tabla con algo que no es una lista de valores");
   }
   vector<Value>& elist = init.as<VectorValue>();
   if (elist.size() > _sz) {
      _error("Demasiados valores al inicializar la tabla");
   }
   vector<Value> *array = new vector<Value>(_sz);
   for (int i = 0; i < elist.size(); i++) {
      (*array)[i] = _celltype->convert(elist[i]);
      /*
      if (elist[i].has_type(_celltype)) {
         ostringstream S;
         S << "La inicialización de la casilla " << i 
           << " tiene tipo '" << elist[i].type_name() << "'" 
           << " cuando debería ser '" << _celltype->name() << "'";
         _error(S.str());
      }
      */
   }
   for (int i = elist.size(); i < array->size(); i++) {
      (*array)[i] = _celltype->create();
   }
   return Value(this, array);
}

Value Struct::create() {
   SimpleTable<Value> *tab = new SimpleTable<Value>();
   for (int i = 0; i < _fields.size(); i++) {
      pair<std::string, Type *> f = _fields[i];
      tab->set(f.first, f.second->create());
   }
   return Value(this, tab);
}

Value Struct::convert(Value init) {
   if (init.has_type(this)) {
      return init.clone();
   }
   if (init.is<VectorValue>()) {
      vector<Value>& values = init.as<VectorValue>();
      if (values.size() > _fields.size()) {
         _error("Demasiados valores al inicializar la tupla de tipo '" + _name + "'");
      }
      SimpleTable<Value> *tab = new SimpleTable<Value>();
      int k = 0;
      for (int i = 0; i < _fields.size(); i++) {
         pair<std::string, Type *> f = _fields[i];
         tab->set(f.first, (i < values.size() 
                            ? f.second->convert(values[i])
                            : f.second->create()));
      }
      return Value(this, tab);
   }
   _error("Para inicializar una tupla hace falta otra tupla igual"
          " o una lista de expresiones entre '{' y '}'");
   return Value::null;
}

void *Struct::clone(void *data) const {
   // The copy constructor in SimpleTable<Value> doesn't clone Values
   // which is what we want here
   //
   SimpleTable<Value> *from = static_cast<SimpleTable<Value>*>(data);
   SimpleTable<Value> *to   = new SimpleTable<Value>();
   for (int i = 0; i < from->size(); i++) {
      const pair<string, Value>& f = (*from)[i];
      to->set(f.first, f.second.clone());
   }
   return to;
}

string Function::typestr() const {
   ostringstream o;
   o << "func(";
   for (int i = 0; i < _param_types.size(); i++) {
      if (i > 0) {
         o << ",";
      }
      o << _param_types[i]->typestr();
   }
   o << ")";
   if (_return_type) {
      o << ":" << _return_type->typestr();
   }
   return o.str();
}

void FuncValue::invoke(Interpreter *I, const std::vector<Value>& args) {
   ptr->invoke(I, args); 
}

template<class MethodMap>
bool _get_method(MethodMap& methods, string name, pair<Type *, Type::Method>& result) {
   auto it = methods.find(name);
   if (it == methods.end()) {
      return false;
   }
   result.first = (it->second.first)();
   result.second = it->second.second;
   return true;
}


bool Vector::get_method(string name, pair<Type*, Method>& result) const {
   auto it = _methods.find(name);
   if (it == _methods.end()) {
      return false;
   }
   result.first = (it->second.first)(_celltype);
   result.second = it->second.second;
   return true;
}

bool String::get_method(string name, pair<Type*, Method>& result) const {
   return _get_method(_methods, name, result);
}

// Methods ////////////////////////////////////////////////////////////

// Vector
map<string, pair<std::function<Type *(Type *)>, Type::Method>> Vector::_methods = {
   {
      "size", {
         // creates the type for 'size'
         [](Type *celltype) -> Function * { 
            return new Function(Int::self); 
         },
         // executes the 'size' method
         [](void *data, const vector<Value>& args) -> Value {
            assert(args.empty());
            vector<Value> *v = static_cast<vector<Value>*>(data);
            return Value(int(v->size()));
         }
      }
   }, {
      "push_back", {
         // creates the type for 'size'
         [](Type *celltype) -> Function * { 
            return (new Function(0))->add_param(celltype);
         },
         // executes the 'push_back' method
         [](void *data, const vector<Value>& args) -> Value {
            vector<Value> *v = static_cast<vector<Value>*>(data);
            v->push_back(args[0]);
            return Value::null;
         }
      }
   }, {
      "resize", {
         [](Type *celltype) -> Function * {
            return (new Function(0))->add_param(Int::self);
         },
         [](void *data, const vector<Value>& args) -> Value {
            vector<Value> *v = static_cast<vector<Value>*>(data);
            v->resize(args[0].as<Int>());
            return Value::null;
         }
      }
   }, {
      "front", {
         [](Type *celltype) -> Function * {
            return (new Function(celltype));
         },
         [](void *data, const vector<Value>& args) -> Value {
            vector<Value> *v = static_cast<vector<Value>*>(data);
            return Reference::mkref(v->front());
         }
      }
   }, {
      "back", {
         [](Type *celltype) -> Function * {
            return (new Function(celltype));
         },
         [](void *data, const vector<Value>& args) -> Value {
            vector<Value> *v = static_cast<vector<Value>*>(data);
            return Reference::mkref(v->back());
         }
      }
   }
};

// String
map<string, pair<std::function<Type *()>, Type::Method>> String::_methods = {
   {
      "size", 
      {
         []() -> Type * {
            return new Function(Int::self);
         },
         [](void *data, const vector<Value>& args) -> Value {
            string *s = static_cast<string*>(data);
            return Value(int(s->size()));
         }
      }
   }, {
      "substr",
      {
         []() -> Type * {
            return (new Function(Int::self))->add_params(Int::self, Int::self);
         },
         [](void *data, const vector<Value>& args) -> Value {
            string *s = static_cast<string*>(data);
            return Value(string(s->substr(args[0].as<Int>(), args[1].as<Int>())));
         }
      }
   }
};

Type *Vector::instantiate(vector<Type *>& subtypes) const {
   assert(subtypes.size() == 1);
   return new Vector(subtypes[0]);
}

string Vector::typestr() const {
   if (_celltype == 0) {
      return "vector";
   } else {
      return std::string("vector<") + _celltype->typestr() + ">"; 
   }
}

string Environment::to_json() const {
   ostringstream json;
   json << "{\"<active>\":" << (_active ? "true" : "false");
   for (int i = 0; i < _tab.tab.size(); i++) {
      if (_tab.tab[i]._hidden) {
         continue;
      }
      json << ",\"" << _tab.tab[i].name() << "\":";
      json << _tab.tab[i].data().to_json();
   }
   json << "}";
   return json.str();
}

void Environment::register_type(string name, Type *type) {
   _curr_namespace.register_type(name, type);
}

Type *Environment::get_type(TypeSpec *spec) {
   Type *type = _curr_namespace.get_type(spec);
   if (type != 0) {
      return type;
   }
   for (Environment *e : _other_namespaces) {
      type = e->get_type(spec);
      if (type != 0) {
         return type;
      }
   }
   return 0;
}

void Environment::using_namespace(Environment *e) {
   _other_namespaces.insert(e);
}

bool Environment::get(string name, Value& res) {
   if (_tab.get(name, res)) {
      return true;
   }
   for (Environment *e : _other_namespaces) {
      if (e->get(name, res)) {
         return true;
      }
   }
   return false;
}

void Environment::set(string name, Value data, bool hidden) {
   _tab.set(name, data, hidden);
}
