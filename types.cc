
#include <vector>
#include <sstream>
using namespace std;

#include "types.hh"
#include "translator.hh"

void _error(std::string msg) {
   throw TypeError(msg);
}

// static + Globals
Type        *Void = 0;
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
Overloaded  *Overloaded::self  = new Overloaded();
Callable    *Callable::self    = new Callable();

string String::to_json(void *data) const {
   return string("\"") + *(string*)data + "\"";
}

string Array::to_json(void *data) const {
   ostringstream json;
   json << "[";
   vector<Value> *v = static_cast<vector<Value>*>(data);
   for (int i = 0; i < v->size(); i++) {
      if (i > 0) {
         json << ",";
      }
      json << (*v)[i].to_json();
   }
   json << "]";
   return json.str();
}

Value Cout(cout), Cerr(cerr);
Value Cin(cin);
Value Endl("\n");

// Methods

Type *TypeMap::get_type(TypeSpec *spec, Environment *outer) {
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
            subtypes.push_back(outer->get_type(spec->id->subtypes[i]));
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

Value Reference::convert(Value x) {
   assert(x.is<Reference>());
   Value::Box *b = (Value::Box*)x._box->data;
   b->count++;
   return Value(x._box->type, b);
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

string Reference::to_json(void *data) const {
   Value::Box *b = (Value::Box*)data;
   std::ostringstream O;
   O << "{\"<type>\":\"ref\",\"ref\":\"" << b << "\"}";
   return O.str();
}

// Initializations
Value Int::convert(Value x) {
   x = Reference::deref(x);
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

bool Int::accepts(const Type *t) const {
   if (t->is<Reference>()) {
      t = t->as<Reference>()->subtype();
   }
   return t->is<Int>() or 
      t->is<Float>() or t->is<Double>() or t->is<Char>() or t->is<Bool>();
}

Value Float::convert(Value x) {
   x = Reference::deref(x);
   if (x.is<Float>()) {
      return x.clone();
   } else if (x.is<Int>()) {
      return Value(float(x.as<Int>()));
   } else if (x.is<Double>()) {
      return Value(float(x.as<Double>()));
   }
   return Value::null;
}

bool Float::accepts(const Type *t) const {
   if (t->is<Reference>()) {
      t = t->as<Reference>()->subtype();
   }
   return t->is<Float>() or 
      t->is<Int>() or t->is<Double>() or t->is<Char>() or t->is<Bool>();
}

Value Double::convert(Value x) {
   x = Reference::deref(x);
   if (x.is<Double>()) {
      return x.clone();
   } else if (x.is<Int>()) {
      return Value(float(x.as<Int>()));
   } else if (x.is<Float>()) {
      return Value(float(x.as<Float>()));
   }
   return Value::null;
}

bool Double::accepts(const Type *t) const {
   if (t->is<Reference>()) {
      t = t->as<Reference>()->subtype();
   }
   return t->is<Double>() or 
      t->is<Int>() or t->is<Float>() or t->is<Char>() or t->is<Bool>();
}

Value Char::convert(Value x) {
   x = Reference::deref(x);
   if (x.is<Char>()) {
      return x.clone();
   } else if (x.is<Int>()) {
      return Value(char(x.as<Int>()));
   }
   return Value::null;
}

bool Char::accepts(const Type *t) const {
   if (t->is<Reference>()) {
      t = t->as<Reference>()->subtype();
   }
   return t->is<Char>() or t->is<Int>();
}

string Char::to_json(void *data) const {
   ostringstream json;
   json << "{\"<type>\":\"char\",\"char\":\"" 
        << *(char*)data << "\"}" << endl;
   return json.str();
}

Value Bool::convert(Value x) {
   x = Reference::deref(x);
   if (x.is<Bool>()) {
      return x.clone();
   } else if (x.is<Int>()) {
      return Value(x.as<Int>() > 0);
   }
   return Value::null;
}

bool Bool::accepts(const Type *t) const {
   if (t->is<Reference>()) {
      t = t->as<Reference>()->subtype();
   }
   return t->is<Bool>() or t->is<Int>();
}


Value Vector::convert(Value x) {
   //
   // To support C++11-style initialization of vectors, this method should
   // be like Array::convert...
   //
   x = Reference::deref(x);
   if (x.is<Vector>()) {
      return x.clone();
   }
   return Value::null;
}

Value Vector::default_value_for(Type *t) {
   // Valor por defecto para cada tipo controlado por vector!
   if (t->is<Int>()) {
      return Value(0);
   } else if (t->is<Bool>()) {
      return Value(false);
   } else if (t->is<Float>()) {
      return Value(0.0f);
   } else if (t->is<Double>()) {
      return Value(0.0);
   } else if (t->is<Char>()) {
      return Value('\0');
   } else {
      return t->create();
   }
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
      init = default_value_for(_celltype);
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

string Struct::to_json(void *data) const {
   SimpleTable<Value>& tab = *static_cast<SimpleTable<Value>*>(data);
   ostringstream json;
   json << "{\"<type>\":\"struct\"";
   for (int i = 0; i < tab.size(); i++) {
      json << ",";
      const pair<string, Value>& f = tab[i];
      json << '"' << f.first << "\":" << f.second.to_json();
   }
   json << "}";
   return json.str();
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

/*
void FuncValue::invoke(Interpreter *I, const std::vector<Value>& args) {
   ptr->invoke(I, args); 
}
*/

bool Vector::get_method(string name, vector<Value>& result) const {
   auto it = _methods.find(name);
   if (it == _methods.end()) {
      return false;
   }
   while (it != _methods.end() and it->first == name) {
      result.push_back(it->second);
      it++;
   }
   return true;
}

bool String::get_method(string name, vector<Value>& result) const {
   auto it = _methods.find(name);
   if (it == _methods.end()) {
      return false;
   }
   while (it != _methods.end() and it->first == name) {
      result.push_back(it->second);
      it++;
   }
   return true;
}

// Methods ////////////////////////////////////////////////////////////

void Vector::_add_method(Function *type, Func *f) {
   _methods.insert(make_pair(f->name, type->mkvalue(f)));
}

Vector::Vector(Type *celltype) : _celltype(celltype) {
   // size
   struct SizeMethod : public Func {
      SizeMethod() : Func("size") {}
      Value call(Value self, const vector<Value>& args) {
         assert(args.empty());
         vector<Value>& the_vector = self.as<Vector>();
         return Value(int(the_vector.size()));
      }
   };
   _add_method(new Function(Int::self), 
               new SizeMethod());

   // push_back
   struct PushBackMethod : public Func {
      PushBackMethod() : Func("push_back") {}
      Value call(Value self, const vector<Value>& args) {
         vector<Value>& v = self.as<Vector>();
         v.push_back(args[0]);
         return Value::null;
      }
   };
   _add_method((new Function(Void))->add_param(celltype),
               new PushBackMethod());

   // resize(int)
   struct Resize1Method : public Func {
      Type *celltype;
      Resize1Method(Type *t) : Func("resize"), celltype(t) {}
      Value call(Value self, const vector<Value>& args) {
         vector<Value>& the_vector = self.as<Vector>();
         the_vector.resize(args[0].as<Int>(), default_value_for(celltype));
         return Value::null;
      }
   };
   _add_method((new Function(Void))->add_param(Int::self),
               new Resize1Method(celltype));

   // resize(int, T)
   struct Resize2Method : public Func {
      Resize2Method() : Func("resize") {}
      Value call(Value self, const vector<Value>& args) {
         vector<Value>& the_vector = self.as<Vector>();
         the_vector.resize(args[0].as<Int>(), args[1]);
         return Value::null;
      }
   };
   _add_method((new Function(Void))->add_params(Int::self, celltype),
               new Resize2Method());
   
   // front
   struct FrontMethod : public Func {
      FrontMethod() : Func("front") {}
      Value call(Value self, const vector<Value>& args)  {
         vector<Value>& the_vector = self.as<Vector>();
         return Reference::mkref(the_vector.front());
      }
   };
   _add_method(new Function(celltype),
               new FrontMethod());

   // back
   struct BackMethod : public Func {
      BackMethod() : Func("back") {}
      Value call(Value self, const vector<Value>& args) {
         vector<Value>& the_vector = self.as<Vector>();
         return Reference::mkref(the_vector.back());
      }
   };
   _add_method(new Function(celltype),
               new BackMethod());
}

void String::_add_method(Function *type, Func *f) {
   _methods.insert(make_pair(f->name, type->mkvalue(f)));
}

String::String() : BasicType("string") {
   // size
   struct SizeMethod : public Func {
      SizeMethod() : Func("size") {}
      Value call(Value self, const vector<Value>& args) {
         string& the_string = self.as<String>();
         return Value(int(the_string.size()));
      }
   };
   _add_method(new Function(Int::self),
               new SizeMethod());
   
   // substr
   struct SubstrMethod : public Func {
      SubstrMethod() : Func("substr") {}
      Value call(Value self, const vector<Value>& args) {
         string& the_string = self.as<String>();
         return Value(the_string.substr(args[0].as<Int>(), args[1].as<Int>()));
      }
   };
   _add_method((new Function(String::self))->add_params(Int::self, Int::self),
               new SubstrMethod());
}

Type *Vector::instantiate(vector<Type *>& subtypes) const {
   assert(subtypes.size() == 1);
   // TODO: create the methods and types for them!
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
   json << "{\"name\":\"" << _name << "\",\"tab\":";
   json << "{\"<active>\":" << (_active ? "true" : "false");
   for (int i = 0; i < _tab.tab.size(); i++) {
      if (_tab.tab[i]._hidden) {
         continue;
      }
      json << ",\"" << _tab.tab[i].name() << "\":";
      json << _tab.tab[i].data().to_json();
   }
   json << "}}";
   return json.str();
}

void Environment::register_type(string name, Type *type) {
   _curr_namespace.register_type(name, type);
}

Type *Environment::get_type(TypeSpec *spec) {
   Type *type = _curr_namespace.get_type(spec, this);
   if (type != 0) {
      return type;
   }
   for (Environment *e : _other_namespaces) {
      type = e->get_type(spec);
      if (type != 0) {
         return type;
      }
   }
   if (_parent != 0) {
      return _parent->get_type(spec);
   }
   return 0;
}

void Environment::using_namespace(Environment *e) {
   _other_namespaces.insert(e);
}

void Environment::set_active(bool active) {
   if (active) {
      _active = true;
      if (_parent != 0 and _parent->_active) {
         _parent->_active = false;
      }
   } else {
      _active = false;
   }
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
   if (_parent != 0) {
      return _parent->get(name, res);
   }
   return false;
}

void Environment::set(string name, Value data, bool hidden) {
   _tab.set(name, data, hidden);
}

Environment *Environment::pop() {
   Environment *parent = _parent;
   delete this;
   return parent;
}

Value OverloadedValue::resolve(const std::vector<Value>& args) {
   vector<Value> results;
   for (int i = 0; i < _candidates.size(); i++) {
      Function *ftype = _candidates[i].type()->as<Function>();
      assert(ftype != 0);
      if (ftype->check_args(args)) {
         results.push_back(_candidates[i]);
      }
   }
   if (results.size() > 1) {
      _error(_T("More than one method is applicable"));
   } else if (results.empty()) {
      _error(_T("No method applicable"));
   }
   return Callable::self->mkvalue(_self, results[0]);
}

Value Overloaded::mkvalue(Value self, const vector<Value>& candidates) {
   OverloadedValue *ov = new OverloadedValue();
   ov->_self = self;
   ov->_candidates = candidates;
   return Value(Overloaded::self, ov);
}

bool Function::check_args(const std::vector<Value>& args) const {
   if (args.size() != _param_types.size()) {
      return false;
   }
   for (int i = 0; i < args.size(); i++) {
      if (!_param_types[i]->accepts(args[i].type())) {
         return false;
      }
   }
   return true;
}

