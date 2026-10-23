
#include <sstream>
#include <vector>
using namespace std;
#include "i18n.hh"
#include "types.hh"

void _error(std::string msg) {
    throw TypeError(msg);
}

// static + Globals
const Type          *Void = 0;
const Type          *Any = (const Type *)1;
const UnknownType   *UnknownType::self = new UnknownType();
const Int           *Int::self = new Int();
const Float         *Float::self = new Float();
const Double        *Double::self = new Double();
const Char          *Char::self = new Char();
const Char          *Char::self_ref = new Char(false);
const Bool          *Bool::self = new Bool();
const String        *String::self = new String();
const OStream       *OStream::self = new OStream();
const IStream       *IStream::self = new IStream();
const VectorValue   *VectorValue::self = new VectorValue();
const Vector        *Vector::self = new Vector();
const List          *List::self = new List();
const Pair          *Pair::self = new Pair();
const Map           *Map::self = new Map();
const Overloaded    *Overloaded::self = new Overloaded();
const Callable      *Callable::self = new Callable();
const OStringStream *OStringStream::self = new OStringStream();
const IStringStream *IStringStream::self = new IStringStream();

string String::to_json(void *data) const {
    return string("\"") + Literal::Escape(*(string *)data, '"') + "\"";
}

map<string, const Type *> Type::reference_types;

// Methods
const Type *TypeMap::instantiate_template(
    const vector<TypeSpec *>& subtypespecs,
    const Type               *T,
    Environment              *topmost
) {
    assert(T->is(Type::Template));
    vector<const Type *> subtypes;
    for (int i = 0; i < subtypespecs.size(); i++) {
        subtypes.push_back(topmost->get_type(subtypespecs[i], topmost));
    }
    return T->instantiate(subtypes);
}

const Type *TypeMap::get_type(TypeSpec *spec, Environment *topmost) {
    // 1. If typestr already registered, return the type
    {
        auto it = _typecache.find(spec->TypeStr());
        if (it != _typecache.end()) {
            return it->second;
        }
    }
    // 2. Construct the Type from the TypeSpec
    {
        vector<Identifier *> path = spec->id->GetNonNamespaces();
        assert(!path.empty());
        // find first type
        const Type *T;
        Identifier *spec0 = path[0];
        auto        it = _typecache.find(spec0->TypeStr());
        if (it != _typecache.end()) {
            T = it->second;
        } else {
            auto it = _typemap.find(spec0->name);
            if (it == _typemap.end()) {
                return 0;
            }
            T = it->second;
            if (spec0->is_template()) {
                T = instantiate_template(spec0->subtypes, T, topmost);
            }
        }
        // traverse inner classes
        for (int i = 1; i < path.size(); i++) {
            const Type *inner = T->get_inner_class(path[i]->name);
            if (inner == 0) {
                return 0;
            }
            T = inner;
        }
        if (spec->reference) {
            T = new Reference(T);
        }
        _typecache[spec->TypeStr()] = T;
        return T;
    }
}

void TypeMap::register_type(string name, const Type *typespec) {
    auto it = _typemap.find(name);
    assert(it == _typemap.end() or it->second->TypeStr() == typespec->TypeStr());
    _typemap[name] = typespec;
    _typecache[typespec->TypeStr()] = typespec;
}

void TypeMap::clear() {
    _typemap.clear();
    _typecache.clear();
}

const Type *Type::mkref(const Type *t) {
    const string typestr = t->TypeStr();
    auto         it = reference_types.find(typestr);
    if (it == reference_types.end()) {
        auto res = reference_types.insert(make_pair(typestr, new Reference(t)));
        it = res.first;
    }
    return it->second;
}

Value UnknownType::convert(Value v) const {
    // disregard v completely (to not produce further errors)
    return Value(this, Value::unknown);
}

void Char::destroy(void *data) const {
    if (_destroy) {
        BaseType<char>::destroy(data);
    }
}

void *Reference::alloc(Value& x) const {
    Value::Box *b = x._box;
    b->count++;
    return b;
}

void Reference::destroy(void *data) const {
    if (data == 0) {  // abstract Reference
        return;
    }
    Value::Box *b = (Value::Box *)data;
    if (--(b->count) == 0) {
        b->type->destroy(b->data);
        delete b;
    }
}

Value Reference::convert(Value x) const {
    assert(x.is<Reference>());
    Value::Box *b = (Value::Box *)x._box->data;
    b->count++;
    return Value(x._box->type, b);
}

Value Reference::mkref(Value& v) {
    if (v.is<Reference>()) {
        return v;
    }
    assert(!v.is_null());
    Value::Box *b = v._box;
    v._box->count++;
    return Value(Type::mkref(v._box->type), (void *)b, v._const);
}

Value Reference::create_abstract() const {
    Value v = _subtype->create_abstract();
    return mkref(v);
}

Value Reference::deref(const Value& v) {
    if (v.is<Reference>()) {
        Value::Box *b = (Value::Box *)v._box->data;
        return Value(b, v._const);
    } else {
        return v;
    }
}

void Reference::clear_touched(void *data) const {
    Value::Box *b = (Value::Box *)data;
    assert(b != 0);
    if (b->data != 0) {
        b->type->clear_touched(b->data);
    }
}

string Reference::to_json(void *data) const {
    Value::Box        *b = (Value::Box *)data;
    std::ostringstream O;
    O << "{\"<type>\":\"ref\",\"ref\":\"" << b << "\"}";
    return O.str();
}

template <typename T>
bool BaseType<T>::accepts(const Type *t) const {
    if (t->is<Reference>()) {
        t = t->as<Reference>()->subtype();
    }
    return t == this;
}

// Initializations
Value Int::convert(Value x) const {
    x = Reference::deref(x);
    if (x.is_abstract()) {
        if (x.is<Int>() or x.is<Float>() or x.is<Double>() or x.is<Char>() or x.is<Bool>()) {
            return Value(Int::self, 0);
        }
    } else if (x.is<Int>()) {
        return x.clone();
    } else if (x.is<Float>()) {
        return Value(int(x.as<Float>()));
    } else if (x.is<Double>()) {
        return Value(int(x.as<Double>()));
    } else if (x.is<Char>()) {
        return Value(int(x.as<Char>()));
    }
    return Value::null;
}

bool Int::accepts(const Type *t) const {
    if (t->is<Reference>()) {
        t = t->as<Reference>()->subtype();
    }
    return t->is<Int>() or t->is<Float>() or t->is<Double>() or t->is<Char>() or t->is<Bool>();
}

Value Float::convert(Value x) const {
    x = Reference::deref(x);
    if (x.is_abstract()) {
        if (x.is<Float>() or x.is<Double>() or x.is<Int>()) {
            return Value(Float::self, 0);
        }
    } else if (x.is<Float>()) {
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
    return t->is<Float>() or t->is<Int>() or t->is<Double>() or t->is<Char>() or t->is<Bool>();
}

Value Double::convert(Value x) const {
    x = Reference::deref(x);
    if (x.is_abstract()) {
        if (x.is<Double>() or x.is<Int>() or x.is<Float>()) {
            return Value(Double::self, 0);
        }
    } else if (x.is<Double>()) {
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
    return t->is<Double>() or t->is<Int>() or t->is<Float>() or t->is<Char>() or t->is<Bool>();
}

Value Char::convert(Value x) const {
    x = Reference::deref(x);
    if (x.is_abstract()) {
        if (x.is<Char>() or x.is<Int>()) {
            return Value(Char::self, 0);
        }
    } else if (x.is<Char>()) {
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
    string        ch(1, *(char *)data);
    json << "{\"<type>\":\"char\",\"char\":\"" << Literal::Escape(ch, '"') << "\"}" << endl;
    return json.str();
}

Value Bool::convert(Value x) const {
    x = Reference::deref(x);
    if (x.is_abstract()) {
        if (x.is<Bool>() or x.is<Int>()) {
            return Value(Bool::self, 0);
        }
    } else if (x.is<Bool>()) {
        return x.clone();
    }
    return Value::null;
}

bool Bool::accepts(const Type *t) const {
    if (t->is<Reference>()) {
        t = t->as<Reference>()->subtype();
    }
    return t->is<Bool>() or t->is<Int>();
}

// Vector ////////////////////////////////////////////////////////////
// Valor por defecto para cada tipo según constructores de
// vector, list y map en la STL...
//
Value default_value_for(const Type *t) {
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

Vector::Vector(const Type *celltype) : Class("vector"), _celltype(celltype) {
    // vector(size)
    struct VectorConstructor1 : public Func {
        const Type *celltype;

        VectorConstructor1(const Type *t) : Func("vector"), celltype(t) {}

        Value call(Value self, const vector<Value>& args) {
            vector<Value>& the_vector = self.as<Vector>();
            Value          the_size = Reference::deref(args[0]);
            const int      sz = the_size.as<Int>();
            the_vector.resize(sz);
            for (int i = 0; i < sz; i++) {
                the_vector[i] = default_value_for(celltype);
            }
            return Value::null;
        }
    };

    _add_method((new Function(Void))->add_params(Int::self), new VectorConstructor1(celltype));

    // vector(size, elem)
    struct VectorConstructor2 : public Func {
        VectorConstructor2() : Func("vector") {}

        Value call(Value self, const vector<Value>& args) {
            vector<Value>& the_vector = self.as<Vector>();
            Value          the_size = Reference::deref(args[0]);
            const int      sz = the_size.as<Int>();
            the_vector.resize(sz);
            Value init = Reference::deref(args[1]);
            for (int i = 0; i < sz; i++) {
                the_vector[i] = init.clone();
            }
            return Value::null;
        }
    };

    _add_method((new Function(Void))->add_params(Int::self, celltype), new VectorConstructor2());

    // size
    struct SizeMethod : public Func {
        SizeMethod() : Func("size") {}

        Value call(Value self, const vector<Value>& args) {
            assert(args.empty());
            vector<Value>& the_vector = self.as<Vector>();
            return Value(int(the_vector.size()));
        }
    };

    _add_method(new Function(Int::self), new SizeMethod());

    // empty
    struct EmptyMethod : public Func {
        EmptyMethod() : Func("empty") {}

        Value call(Value self, const vector<Value>& args) {
            assert(args.empty());
            vector<Value>& the_vector = self.as<Vector>();
            return Value(bool(the_vector.empty()));
        }
    };

    _add_method(new Function(Bool::self), new EmptyMethod());

    // push_back
    struct PushBackMethod : public Func {
        PushBackMethod() : Func("push_back") {}

        Value call(Value self, const vector<Value>& args) {
            vector<Value>& v = self.as<Vector>();
            Value          pushed = Reference::deref(args[0]);
            v.push_back(pushed.clone());
            return Value::null;
        }
    };

    _add_method((new Function(Void))->add_params(celltype), new PushBackMethod());

    // pop_back
    struct PopBackMethod : public Func {
        PopBackMethod() : Func("pop_back") {}

        Value call(Value self, const vector<Value>& args) {
            vector<Value>& the_vector = self.as<Vector>();
            the_vector.pop_back();
            return Value::null;
        }
    };

    _add_method((new Function(Void)), new PopBackMethod());

    // resize(int)
    struct Resize1Method : public Func {
        const Type *celltype;

        Resize1Method(const Type *t) : Func("resize"), celltype(t) {}

        Value call(Value self, const vector<Value>& args) {
            vector<Value>& the_vector = self.as<Vector>();
            Value          the_new_size = Reference::deref(args[0]);
            the_vector.resize(the_new_size.as<Int>(), default_value_for(celltype));
            return Value::null;
        }
    };

    _add_method((new Function(Void))->add_params(Int::self), new Resize1Method(celltype));

    // resize(int, T)
    struct Resize2Method : public Func {
        Resize2Method() : Func("resize") {}

        Value call(Value self, const vector<Value>& args) {
            vector<Value>& the_vector = self.as<Vector>();
            Value          the_new_size = Reference::deref(args[0]);
            Value          the_new_value = Reference::deref(args[1]);
            the_vector.resize(the_new_size.as<Int>(), the_new_value);
            return Value::null;
        }
    };

    _add_method(
        (new Function(Void))->add_params(Int::self, celltype),  // literals!!
        new Resize2Method()
    );
    _add_method(
        (new Function(Void))->add_params(Int::self, new Reference(celltype)), new Resize2Method()
    );

    // front
    struct FrontMethod : public Func {
        FrontMethod() : Func("front") {}

        Value call(Value self, const vector<Value>& args) {
            vector<Value>& the_vector = self.as<Vector>();
            return Reference::mkref(the_vector.front());
        }
    };

    _add_method(new Function(celltype), new FrontMethod());

    // back
    struct BackMethod : public Func {
        BackMethod() : Func("back") {}

        Value call(Value self, const vector<Value>& args) {
            vector<Value>& the_vector = self.as<Vector>();
            return Reference::mkref(the_vector.back());
        }
    };

    _add_method(new Function(celltype), new BackMethod());

    // clear
    struct ClearMethod : public Func {
        ClearMethod() : Func("clear") {}

        Value call(Value self, const vector<Value>& args) {
            vector<Value>& the_vector = self.as<Vector>();
            the_vector.clear();
            return Value::null;
        }
    };

    _add_method(new Function(Void), new ClearMethod());
    // Iterator type + methods
    typedef RandomAccessIterator<Vector> MyIterator;
    Type                                *iterator_type = new MyIterator(this);
    _add_inner_class(iterator_type);

    // begin
    struct BeginMethod : public Func {
        const Type *iter_type;

        BeginMethod(const Type *t) : Func("begin"), iter_type(t) {}

        Value call(Value self, const vector<Value>& args) {
            vector<Value>& the_vector = self.as<Vector>();
            return Value(iter_type, new vector<Value>::iterator(the_vector.begin()));
        }
    };

    _add_method(new Function(iterator_type), new BeginMethod(iterator_type));

    // end
    struct EndMethod : public Func {
        const Type *iter_type;

        EndMethod(const Type *t) : Func("end"), iter_type(t) {}

        Value call(Value self, const vector<Value>& args) {
            vector<Value>& the_vector = self.as<Vector>();
            return Value(iter_type, new vector<Value>::iterator(the_vector.end()));
        }
    };

    _add_method(new Function(iterator_type), new EndMethod(iterator_type));

    // insert
    struct InsertMethod : public Func {
        const Type *iter_type;

        InsertMethod(const Type *t) : Func("insert"), iter_type(t) {}

        Value call(Value self, const vector<Value>& args) {
            vector<Value>&          the_vector = self.as<Vector>();
            Value                   pos = Reference::deref(args[0]);
            Value                   val = Reference::deref(args[1]);
            vector<Value>::iterator it = pos.as<MyIterator>();
            vector<Value>::iterator result = the_vector.insert(it, val);
            return Value(iter_type, new vector<Value>::iterator(result));
        }
    };

    _add_method(
        (new Function(iterator_type))->add_params(iterator_type, celltype),
        new InsertMethod(iterator_type)
    );

    // erase
    struct EraseMethod : public Func {
        const Type *iter_type;

        EraseMethod(const Type *t) : Func("erase"), iter_type(t) {}

        Value call(Value self, const vector<Value>& args) {
            vector<Value>&          the_vector = self.as<Vector>();
            Value                   pos = Reference::deref(args[0]);
            vector<Value>::iterator it = pos.as<MyIterator>();
            vector<Value>::iterator result = the_vector.erase(it);
            return Value(iter_type, new vector<Value>::iterator(result));
        }
    };

    _add_method(
        (new Function(iterator_type))->add_params(iterator_type), new EraseMethod(iterator_type)
    );

    // []
    struct IndexedAccessOperator : public Func {
        IndexedAccessOperator() : Func("[]") {}

        Value call(Value self, const vector<Value>& args) {
            vector<Value>& the_vector = self.as<Vector>();
            Value          the_index = Reference::deref(args[0]);
            int            k = the_index.as<Int>();
            if (k < 0 or k >= the_vector.size()) {
                throw Error("Acceso fuera de rango");  // FIXME
            }
            return Reference::mkref(the_vector[k]);
        }

        bool call_abstract(AstNode *x, Value self, const vector<Value>& args) {
            Value the_index = Reference::deref(args[0]);
            if (!the_index.is<Int>()) {
                x->add_error(
                    _T("El índice a una casilla de un vector debe ser un 'int' (no '%s').",
                       the_index.type()->name().c_str())
                );
            }
            return true;
        }
    };

    _add_method(
        (new Function(Type::mkref(celltype)))->add_params(Int::self), new IndexedAccessOperator()
    );
}

const Type *Vector::instantiate(vector<const Type *>& subtypes) const {
    assert(subtypes.size() == 1);
    assert(subtypes[0] != 0);
    return new Vector(subtypes[0]);
}

string Vector::TypeStr() const {
    string subtype = (_celltype != 0 ? _celltype->TypeStr() : "?");
    return name() + "<" + subtype + ">";
}

Value Vector::convert(Value x) const {
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

string Vector::to_json(void *data) const {
    ostringstream o;
    o << "[";
    vector<Value>& vec = *(vector<Value> *)data;
    for (int i = 0; i < vec.size(); i++) {
        if (i > 0) {
            o << ", ";
        }
        o << vec[i].to_json();
    }
    o << "]";
    return o.str();
}

void Vector::clear_touched(void *data) const {
    vector<Value>& vec = *(vector<Value> *)data;
    for (int i = 0; i < vec.size(); i++) {
        vec[i].clear_touched();
    }
}

// List //////////////////////////////////////////////////////////////
List::List(const Type *celltype) : Class("list"), _celltype(celltype) {
    // list(size)
    struct ListConstructor1 : public Func {
        const Type *celltype;

        ListConstructor1(const Type *t) : Func("list"), celltype(t) {}

        Value call(Value self, const vector<Value>& args) {
            list<Value>& the_list = self.as<List>();
            Value        the_new_size = Reference::deref(args[0]);
            the_list.resize(the_new_size.as<Int>());
            for (Value& elem : the_list) {
                elem = default_value_for(celltype);
            }
            return Value::null;
        }
    };

    _add_method((new Function(Void))->add_params(Int::self), new ListConstructor1(celltype));

    // list(size, elem)
    struct ListConstructor2 : public Func {
        ListConstructor2() : Func("list") {}

        Value call(Value self, const vector<Value>& args) {
            list<Value>& the_list = self.as<List>();
            Value        the_new_size = Reference::deref(args[0]);
            Value        the_new_value = Reference::deref(args[1]);
            the_list.resize(the_new_size.as<Int>());
            Value init = Reference::deref(the_new_value);
            for (Value& elem : the_list) {
                elem = init.clone();
            }
            return Value::null;
        }
    };

    _add_method((new Function(Void))->add_params(Int::self, celltype), new ListConstructor2());

    // size
    struct SizeMethod : public Func {
        SizeMethod() : Func("size") {}

        Value call(Value self, const vector<Value>& args) {
            assert(args.empty());
            list<Value>& the_list = self.as<List>();
            return Value(int(the_list.size()));
        }
    };

    _add_method(new Function(Int::self), new SizeMethod());

    // empty
    struct EmptyMethod : public Func {
        EmptyMethod() : Func("empty") {}

        Value call(Value self, const vector<Value>& args) {
            assert(args.empty());
            list<Value>& the_list = self.as<List>();
            return Value(bool(the_list.empty()));
        }
    };

    _add_method(new Function(Bool::self), new EmptyMethod());

    // push_back
    struct PushBackMethod : public Func {
        PushBackMethod() : Func("push_back") {}

        Value call(Value self, const vector<Value>& args) {
            list<Value>& the_list = self.as<List>();
            Value        pushed = Reference::deref(args[0]);
            the_list.push_back(pushed.clone());
            return Value::null;
        }
    };

    _add_method((new Function(Void))->add_params(celltype), new PushBackMethod());

    // push_front
    struct PushFrontMethod : public Func {
        PushFrontMethod() : Func("push_front") {}

        Value call(Value self, const vector<Value>& args) {
            list<Value>& the_list = self.as<List>();
            Value        pushed = Reference::deref(args[0]);
            the_list.push_front(pushed.clone());
            return Value::null;
        }
    };

    _add_method((new Function(Void))->add_params(celltype), new PushFrontMethod());

    // pop_back
    struct PopBackMethod : public Func {
        PopBackMethod() : Func("pop_back") {}

        Value call(Value self, const vector<Value>& args) {
            list<Value>& the_list = self.as<List>();
            the_list.pop_back();
            return Value::null;
        }
    };

    _add_method((new Function(Void)), new PopBackMethod());

    // pop_front
    struct PopFrontMethod : public Func {
        PopFrontMethod() : Func("pop_front") {}

        Value call(Value self, const vector<Value>& args) {
            list<Value>& the_list = self.as<List>();
            the_list.pop_front();
            return Value::null;
        }
    };

    _add_method((new Function(Void)), new PopFrontMethod());

    // resize(int)
    struct Resize1Method : public Func {
        const Type *celltype;

        Resize1Method(const Type *t) : Func("resize"), celltype(t) {}

        Value call(Value self, const vector<Value>& args) {
            list<Value>& the_list = self.as<List>();
            Value        the_new_size = Reference::deref(args[0]);
            int          sz = the_new_size.as<Int>();
            // emulate the resize method to clone values
            if (sz < the_list.size()) {
                the_list.resize(sz);
            } else {
                for (int i = the_list.size(); i < sz; i++) {
                    the_list.push_back(default_value_for(celltype));
                }
            }
            return Value::null;
        }
    };

    _add_method((new Function(Void))->add_params(Int::self), new Resize1Method(celltype));

    // resize(int, T)
    struct Resize2Method : public Func {
        Resize2Method() : Func("resize") {}

        Value call(Value self, const vector<Value>& args) {
            list<Value>& the_list = self.as<List>();
            Value        the_new_size = Reference::deref(args[0]);
            Value        the_new_value = Reference::deref(args[1]);
            int          sz = the_new_size.as<Int>();
            // emulate the resize method to clone values
            if (sz < the_list.size()) {
                the_list.resize(sz);
            } else {
                for (int i = the_list.size(); i < sz; i++) {
                    the_list.push_back(the_new_value.clone());
                }
            }
            return Value::null;
        }
    };

    _add_method(
        (new Function(Void))->add_params(Int::self, celltype),  // literals!
        new Resize2Method()
    );
    _add_method(
        (new Function(Void))->add_params(Int::self, new Reference(celltype)), new Resize2Method()
    );

    // front
    struct FrontMethod : public Func {
        FrontMethod() : Func("front") {}

        Value call(Value self, const vector<Value>& args) {
            list<Value>& the_list = self.as<List>();
            return Reference::mkref(the_list.front());
        }
    };

    _add_method(new Function(celltype), new FrontMethod());

    // back
    struct BackMethod : public Func {
        BackMethod() : Func("back") {}

        Value call(Value self, const vector<Value>& args) {
            list<Value>& the_list = self.as<List>();
            return Reference::mkref(the_list.back());
        }
    };

    _add_method(new Function(celltype), new BackMethod());

    // clear
    struct ClearMethod : public Func {
        ClearMethod() : Func("clear") {}

        Value call(Value self, const vector<Value>& args) {
            list<Value>& the_list = self.as<List>();
            the_list.clear();
            return Value::null;
        }
    };

    _add_method(new Function(Void), new ClearMethod());

    // reverse
    struct ReverseMethod : public Func {
        ReverseMethod() : Func("reverse") {}

        Value call(Value self, const vector<Value>& args) {
            list<Value>& the_list = self.as<List>();
            the_list.reverse();
            return Value::null;
        }
    };

    _add_method(new Function(Void), new ReverseMethod());

    // unique
    struct UniqueMethod : public Func {
        UniqueMethod() : Func("unique") {}

        Value call(Value self, const vector<Value>& args) {
            list<Value>& the_list = self.as<List>();
            the_list.unique([](const Value& a, const Value& b) { return a.equals(b); });
            return Value::null;
        }
    };

    _add_method(new Function(Void), new UniqueMethod());

    // sort
    struct SortMethod : public Func {
        SortMethod() : Func("sort") {}

        Value call(Value self, const vector<Value>& args) {
            list<Value>& the_list = self.as<List>();
            the_list.sort([](const Value& a, const Value& b) { return a.less_than(b); });
            return Value::null;
        }
    };

    _add_method(new Function(Void), new SortMethod());

    // sort
    struct SortFnMethod : public Func {
        SortFnMethod() : Func("sort") {}

        Value call(Value self, const vector<Value>& args) {
            list<Value>& the_list = self.as<List>();
            Value        the_function = Reference::deref(args[0]);
            the_list.sort([&](const Value& a, const Value& b) {
                vector<Value>   args = {a, b};
                Binding&        fn = the_function.as<Callable>();
                const Function *func_type = fn.func.type()->as<Function>();
                Value           ret = fn.call(args);
                return ret.as<Bool>();
            });
            return Value::null;
        }
    };

    _add_method((new Function(Void))->add_params(Callable::self), new SortFnMethod());
    // Iterator type + methods
    typedef BidirectionalIterator<List> MyIterator;
    Type                               *iterator_type = new BidirectionalIterator<List>(this);
    _add_inner_class(iterator_type);

    // begin
    struct BeginMethod : public Func {
        const Type *iter_type;

        BeginMethod(const Type *t) : Func("begin"), iter_type(t) {}

        Value call(Value self, const vector<Value>& args) {
            list<Value>& the_list = self.as<List>();
            return Value(iter_type, new list<Value>::iterator(the_list.begin()));
        }
    };

    _add_method(new Function(iterator_type), new BeginMethod(iterator_type));

    // end
    struct EndMethod : public Func {
        const Type *iter_type;

        EndMethod(const Type *t) : Func("end"), iter_type(t) {}

        Value call(Value self, const vector<Value>& args) {
            list<Value>& the_list = self.as<List>();
            return Value(iter_type, new list<Value>::iterator(the_list.end()));
        }
    };

    _add_method(new Function(iterator_type), new EndMethod(iterator_type));

    // insert
    struct InsertMethod : public Func {
        const Type *iter_type;

        InsertMethod(const Type *t) : Func("insert"), iter_type(t) {}

        Value call(Value self, const vector<Value>& args) {
            list<Value>&          the_list = self.as<List>();
            Value                 pos = Reference::deref(args[0]);
            list<Value>::iterator it = pos.as<MyIterator>();
            list<Value>::iterator result = the_list.insert(it, args[1]);
            return Value(iter_type, new list<Value>::iterator(result));
        }
    };

    _add_method(
        (new Function(iterator_type))->add_params(iterator_type, celltype),
        new InsertMethod(iterator_type)
    );

    // erase
    struct EraseMethod : public Func {
        const Type *iter_type;

        EraseMethod(const Type *t) : Func("erase"), iter_type(t) {}

        Value call(Value self, const vector<Value>& args) {
            list<Value>&          the_list = self.as<List>();
            Value                 pos = Reference::deref(args[0]);
            list<Value>::iterator it = pos.as<MyIterator>();
            list<Value>::iterator result = the_list.erase(it);
            return Value(iter_type, new list<Value>::iterator(result));
        }
    };

    _add_method(
        (new Function(iterator_type))->add_params(iterator_type), new EraseMethod(iterator_type)
    );
}

const Type *List::instantiate(vector<const Type *>& subtypes) const {
    assert(subtypes.size() == 1);
    assert(subtypes[0] != 0);
    return new List(subtypes[0]);
}

string List::TypeStr() const {
    string subtype = (_celltype != 0 ? _celltype->TypeStr() : "?");
    return name() + "<" + subtype + ">";
}

Value List::convert(Value x) const {
    //
    // To support C++11-style initialization of vectors, this method should
    // be like Array::convert...
    //
    x = Reference::deref(x);
    if (x.is<List>()) {
        return x.clone();
    }
    return Value::null;
}

string List::to_json(void *data) const {
    ostringstream json;
    json << "{\"<type>\":\"list\",\"<elements>\":[";
    list<Value>& lst = *(list<Value> *)data;
    for (auto it = lst.begin(); it != lst.end(); it++) {
        if (it != lst.begin()) {
            json << ", ";
        }
        json << it->to_json();
    }
    json << "]}";
    return json.str();
}

void List::clear_touched(void *data) const {
    list<Value>& lst = *(list<Value> *)data;
    for (auto it = lst.begin(); it != lst.end(); it++) {
        it->clear_touched();
    }
}

// Pair //////////////////////////////////////////////////////////////
bool pair_less_than(const pair<Value, Value>& a, const pair<Value, Value>& b) {
    if (a.first.less_than(b.first)) {
        return true;
    } else if (b.first.less_than(a.first)) {
        return false;
    } else {
        return a.second.less_than(b.second);
    }
}

Pair::Pair(const Type *_1, const Type *_2) : Class("pair"), _first(_1), _second(_2) {
    // <
    struct LessThanOperator : public Func {
        LessThanOperator() : Func("<") {}

        Value call(Value self, const vector<Value>& args) {
            Value               other = Reference::deref(args[0]);
            pair<Value, Value>& a = self.as<Pair>();
            pair<Value, Value>& b = other.as<Pair>();
            return Value(pair_less_than(a, b));
        }
    };

    Base::_add_method((new Function(Bool::self))->add_params(this), new LessThanOperator());
}

bool Pair::less_than(void *a, void *b) const {
    assert(a != 0 and b != 0);
    pair<Value, Value>& A = *static_cast<pair<Value, Value> *>(a);
    pair<Value, Value>& B = *static_cast<pair<Value, Value> *>(b);
    return pair_less_than(A, B);
}

int Pair::get_field(Value self, std::string name, std::vector<Value>& result) const {
    pair<Value, Value> the_pair = self.as<Pair>();
    if (name == "first") {
        result.push_back(the_pair.first);
        return 1;
    } else if (name == "second") {
        result.push_back(the_pair.second);
        return 1;
    }
    return Base::get_field(self, name, result);
}

Value Pair::convert(Value x) const {
    x = Reference::deref(x);
    if (x.is<Pair>()) {
        return x.clone();
    }
    if (x.is<VectorValue>()) {
        vector<Value>& values = x.as<VectorValue>();
        if (values.size() > 2) {
            _error("Demasiados valores al inicializar un '" + name() + "'");
        }
        pair<Value, Value> *p = new pair<Value, Value>();
        p->first = (values.size() >= 1 ? _first->convert(values[0]) : _first->create());
        p->second = (values.size() == 2 ? _second->convert(values[1]) : _second->create());
        return Value(this, p);
    }
    return Value::null;
}

const Type *Pair::instantiate(vector<const Type *>& subtypes) const {
    assert(subtypes.size() == 2);
    assert(subtypes[0] != 0);
    assert(subtypes[1] != 0);
    return new Pair(subtypes[0], subtypes[1]);
}

std::string Pair::TypeStr() const {
    string _1 = (_first != 0 ? _first->TypeStr() : "?");
    string _2 = (_second != 0 ? _second->TypeStr() : "?");
    return string("pair<") + _1 + "," + _2 + ">";
}

std::string Pair::to_json(void *data) const {
    pair<Value, Value> *p = (pair<Value, Value> *)data;
    ostringstream       json;
    json << "{\"<type>\":\"pair\","
         << "\"first\":" << p->first.to_json() << ","
         << "\"second\":" << p->second.to_json() << "}";
    return json.str();
}

// Map ///////////////////////////////////////////////////////////////
Map::Map(const Type *k, const Type *v) : Class("map"), _key(k), _value(v) {
    _pair_type = new Pair(_key, _value);

    // size
    struct SizeMethod : public Func {
        SizeMethod() : Func("size") {}

        Value call(Value self, const vector<Value>& args) {
            assert(args.empty());
            map<Value, Value>& the_map = self.as<Map>();
            return Value(int(the_map.size()));
        }
    };

    _add_method(new Function(Int::self), new SizeMethod());

    // empty
    struct EmptyMethod : public Func {
        EmptyMethod() : Func("empty") {}

        Value call(Value self, const vector<Value>& args) {
            assert(args.empty());
            map<Value, Value>& the_map = self.as<Map>();
            return Value(bool(the_map.empty()));
        }
    };

    _add_method(new Function(Bool::self), new EmptyMethod());

    // clear
    struct ClearMethod : public Func {
        ClearMethod() : Func("clear") {}

        Value call(Value self, const vector<Value>& args) {
            assert(args.empty());
            map<Value, Value>& the_map = self.as<Map>();
            the_map.clear();
            return Value::null;
        }
    };

    _add_method(new Function(Void), new ClearMethod());
    // iterator type
    typedef BidirectionalIterator<Map> MyIterator;
    Type                              *iterator_type = new BidirectionalIterator<Map>(this);
    _add_inner_class(iterator_type);

    // insert
    struct InsertMethod : public Func {
        const Type *iterator_type, *insert_return_type;

        InsertMethod(const Type *t1, const Type *t2)
            : Func("insert"), iterator_type(t1), insert_return_type(t2) {}

        Value call(Value self, const vector<Value>& args) {
            map<Value, Value>&                              the_map = self.as<Map>();
            Value                                           elem = Reference::deref(args[0]);
            typedef pair<map<Value, Value>::iterator, bool> _pair;
            _pair                                           res = the_map.insert(elem.as<Pair>());
            pair<Value, Value>                             *vres = new pair<Value, Value>();
            vres->first = Value(iterator_type, new map<Value, Value>::iterator(res.first));
            vres->second = Value(res.second);
            return Value(insert_return_type, vres);
        }
    };

    const Type *insert_return_type = new Pair(iterator_type, Bool::self);
    _add_method(
        (new Function(insert_return_type))->add_params(_pair_type),
        new InsertMethod(iterator_type, insert_return_type)
    );

    // find
    struct FindMethod : public Func {
        const Type *iterator_type;

        FindMethod(const Type *t) : Func("find"), iterator_type(t) {}

        Value call(Value self, const vector<Value>& args) {
            map<Value, Value>&          the_map = self.as<Map>();
            Value                       key = Reference::deref(args[0]);
            map<Value, Value>::iterator it = the_map.find(key);
            return Value(iterator_type, new map<Value, Value>::iterator(it));
        }
    };

    _add_method((new Function(iterator_type))->add_params(_key), new FindMethod(iterator_type));

    // erase(key)
    struct EraseKeyMethod : public Func {
        EraseKeyMethod() : Func("erase") {}

        Value call(Value self, const vector<Value>& args) {
            map<Value, Value>& the_map = self.as<Map>();
            Value              key = Reference::deref(args[0]);
            the_map.erase(key);
            return Value::null;
        }
    };

    _add_method((new Function(Void))->add_params(_key), new EraseKeyMethod());

    // erase(iterator)
    struct EraseIteratorMethod : public Func {
        EraseIteratorMethod() : Func("erase") {}

        Value call(Value self, const vector<Value>& args) {
            map<Value, Value>& the_map = self.as<Map>();
            Value              the_iterator = Reference::deref(args[0]);
            the_map.erase(the_iterator.as<MyIterator>());
            return Value::null;
        }
    };

    _add_method((new Function(Void))->add_params(iterator_type), new EraseIteratorMethod());

    // begin
    struct BeginMethod : public Func {
        const Type *iter_type;

        BeginMethod(const Type *t) : Func("begin"), iter_type(t) {}

        Value call(Value self, const vector<Value>& args) {
            map<Value, Value>& the_map = self.as<Map>();
            return Value(iter_type, new map<Value, Value>::iterator(the_map.begin()));
        }
    };

    _add_method(new Function(iterator_type), new BeginMethod(iterator_type));

    // end
    struct EndMethod : public Func {
        const Type *iter_type;

        EndMethod(const Type *t) : Func("end"), iter_type(t) {}

        Value call(Value self, const vector<Value>& args) {
            map<Value, Value>& the_map = self.as<Map>();
            return Value(iter_type, new map<Value, Value>::iterator(the_map.end()));
        }
    };

    _add_method(new Function(iterator_type), new EndMethod(iterator_type));

    // []
    struct FindOperator : public Func {
        const Type *value_type;

        FindOperator(const Type *t) : Func("[]"), value_type(t) {}

        Value call(Value self, const vector<Value>& args) {
            map<Value, Value>& the_map = self.as<Map>();
            // Each key has to be a new object, otherwise the box of a key
            // could be modified outside the map and the behavior of the map
            // would be chaotic.
            Value  key = Reference::deref(args[0]).clone();  // clone the key and use the clone
            Value& val = the_map[key];
            if (val == Value::null) {
                val = default_value_for(value_type);
            }
            return Reference::mkref(val);
        }
    };

    _add_method((new Function(_value))->add_params(_key), new FindOperator(_value));
}

const Type *Map::instantiate(vector<const Type *>& subtypes) const {
    assert(subtypes.size() == 2);
    assert(subtypes[0] != 0);
    assert(subtypes[1] != 0);
    return new Map(subtypes[0], subtypes[1]);
}

std::string Map::TypeStr() const {
    string _1 = (_key != 0 ? _key->TypeStr() : "?");
    string _2 = (_value != 0 ? _value->TypeStr() : "?");
    return string("map<") + _1 + "," + _2 + ">";
}

std::string Map::to_json(void *data) const {
    map<Value, Value>& the_map = *(map<Value, Value> *)data;
    ostringstream      json;
    json << "{\"<type>\":\"map\",\"<elements>\":[";
    for (auto it = the_map.begin(); it != the_map.end(); it++) {
        if (it != the_map.begin()) {
            json << ", ";
        }
        json << "{\"key\":" << it->first.to_json() << "\", "
             << "\"value\":" << it->second.to_json() << "}";
    }
    json << "]}";
    return json.str();
}

// Array /////////////////////////////////////////////////////////////
const Type *
Array::_mkarray(const Type *celltype, vector<int>::const_iterator curr, const vector<int>& sizes) {
    if (curr == sizes.end()) {
        return celltype;
    } else {
        int sz = *curr++;
        return new Array(_mkarray(celltype, curr, sizes), sz);
    }
}

const Type *Array::mkarray(const Type *celltype, const vector<int>& sizes) {
    return _mkarray(celltype, sizes.begin(), sizes);
}

string Array::to_json(void *data) const {
    ostringstream json;
    json << "[";
    vector<Value> *v = static_cast<vector<Value> *>(data);
    for (int i = 0; i < v->size(); i++) {
        if (i > 0) {
            json << ",";
        }
        json << (*v)[i].to_json();
    }
    json << "]";
    return json.str();
}

void Array::clear_touched(void *data) const {
    vector<Value> *v = static_cast<vector<Value> *>(data);
    for (int i = 0; i < v->size(); i++) {
        (*v)[i].clear_touched();
    }
}

bool Array::contains_unknowns(void *data) const {
    vector<Value> *v = static_cast<vector<Value> *>(data);
    for (int i = 0; i < v->size(); i++) {
        if ((*v)[i].is_unknown() or (*v)[i].contains_unknowns()) {
            return true;
        }
    }
    return false;
}

Value Array::create() const {
    vector<Value> *array = new vector<Value>(_sz);
    for (int i = 0; i < _sz; i++) {
        (*array)[i] = _celltype->create();
    }
    return Value(this, array);
}

Value Array::create_abstract() const {
    vector<Value> *array = new vector<Value>(1);
    (*array)[0] = _celltype->create_abstract();
    return Value(this, array);
}

Value Array::convert(Value init) const {
    assert(!init.is_null());
    if (!init.is<VectorValue>()) {
        _error("Debes inicializar con una lista de valores entre '{' y '}'.");
    }
    vector<Value>& elist = init.as<VectorValue>();
    if (elist.size() > _sz) {
        _error("Demasiados valores al inicializar la tabla.");
    }
    vector<Value> *array = new vector<Value>(_sz);
    for (int i = 0; i < elist.size(); i++) {
        (*array)[i] = _celltype->convert(elist[i]);
    }
    for (int i = elist.size(); i < array->size(); i++) {
        (*array)[i] = _celltype->create();
    }
    return Value(this, array);
}

bool Struct::contains_unknowns(void *data) const {
    Table<Value> *tab = static_cast<Table<Value> *>(data);
    for (int i = 0; i < _fields.size(); i++) {
        pair<std::string, const Type *> f = _fields[i];
        Value                           v;
        if (!tab->get(f.first, v)) {
            return true;
        } else if (v.is_unknown() or v.contains_unknowns()) {
            return true;
        }
    }
    return false;
}

Value Struct::create() const {
    Table<Value> *tab = new Table<Value>();
    for (int i = 0; i < _fields.size(); i++) {
        pair<std::string, const Type *> f = _fields[i];
        tab->set(f.first, f.second->create());
    }
    return Value(this, tab);
}

Value Struct::create_abstract() const {
    Table<Value> *tab = new Table<Value>();
    for (int i = 0; i < _fields.size(); i++) {
        pair<std::string, const Type *> f = _fields[i];
        tab->set(f.first, f.second->create_abstract());
    }
    return Value(this, tab);
}

Value Struct::convert(Value init) const {
    if (init.has_type(this)) {
        return init.clone();
    }
    if (init.is<VectorValue>()) {
        vector<Value>& values = init.as<VectorValue>();
        if (values.size() > _fields.size()) {
            _error("Demasiados valores al inicializar la tupla de tipo '" + name() + "'");
        }
        Table<Value> *tab = new Table<Value>();
        int           k = 0;
        for (int i = 0; i < _fields.size(); i++) {
            pair<std::string, const Type *> f = _fields[i];
            tab->set(
                f.first, (i < values.size() ? f.second->convert(values[i]) : f.second->create())
            );
        }
        return Value(this, tab);
    }
    _error(
        "Para inicializar una tupla hace falta otra tupla igual"
        " o una lista de expresiones entre '{' y '}'"
    );
    return Value::null;
}

void *Struct::clone(void *data) const {
    // The copy constructor in SimpleTable<Value> doesn't clone Values
    // which is what we want here
    //
    Table<Value> *from = static_cast<Table<Value> *>(data);
    Table<Value> *to = new Table<Value>();
    for (int i = 0; i < from->size(); i++) {
        const pair<string, Value>& f = (*from)[i];
        to->set(f.first, f.second.clone());
    }
    return to;
}

void Struct::clear_touched(void *data) const {
    Table<Value> *_tab = static_cast<Table<Value> *>(data);
    for (int i = 0; i < _tab->size(); i++) {
        (*_tab)[i].second.clear_touched();
    }
}

string Struct::to_json(void *data) const {
    Table<Value>& tab = *static_cast<Table<Value> *>(data);
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

string Function::TypeStr() const {
    ostringstream o;
    o << "func(";
    for (int i = 0; i < _param_types.size(); i++) {
        if (i > 0) {
            o << ",";
        }
        o << _param_types[i]->TypeStr();
    }
    o << ")";
    if (_return_type) {
        o << ":" << _return_type->TypeStr();
    }
    return o.str();
}

template <class Base>
void Class<Base>::_add_static(string name, Value v) {
    _statics[name] = v;
}

template <class Base>
bool Class<Base>::get_static(string name, Value& v) const {
    auto it = _statics.find(name);
    if (it != _statics.end()) {
        v = it->second;
        return true;
    }
    return false;
}

template <class Base>
int Class<Base>::get_field(Value self, string name, vector<Value>& result) const {
    auto it = _methods.find(name);
    if (it == _methods.end()) {
        return 0;
    }
    int count = 0;
    while (it != _methods.end() and it->first == name) {
        result.push_back(it->second);
        it++;
        count++;
    }
    return count;
}

template <class Base>
void Class<Base>::_add_method(const Function *type, Func *f) {
    _methods.insert(make_pair(f->name, type->mkvalue(f)));
}

// Methods ////////////////////////////////////////////////////////////
String::String() : Class("string") {
    // constructor(size, char)
    struct StringConstructor1 : public Func {
        StringConstructor1() : Func("string") {}

        Value call(Value self, const vector<Value>& args) {
            string&   the_string = self.as<String>();
            Value     size = Reference::deref(args[0]);
            const int sz = size.as<Int>();
            the_string = "";
            for (int i = 0; i < sz; i++) {
                the_string += args[1].as<Char>();
            }
            return Value::null;
        }
    };

    _add_method((new Function(Void))->add_params(Int::self, Char::self), new StringConstructor1());

    // size
    struct SizeMethod : public Func {
        SizeMethod(string name) : Func(name) {}

        Value call(Value self, const vector<Value>& args) {
            string& the_string = self.as<String>();
            return Value(int(the_string.size()));
        }
    };

    _add_method(new Function(Int::self), new SizeMethod("size"));
    _add_method(new Function(Int::self), new SizeMethod("length"));

    // substr(from, size)
    struct Substr1Method : public Func {
        Substr1Method() : Func("substr") {}

        Value call(Value self, const vector<Value>& args) {
            string& the_string = self.as<String>();
            Value   pos = Reference::deref(args[0]);
            Value   size = Reference::deref(args[1]);
            return Value(the_string.substr(pos.as<Int>(), size.as<Int>()));
        }
    };

    _add_method((new Function(this))->add_params(Int::self, Int::self), new Substr1Method());

    // substr(from)
    struct Substr2Method : public Func {
        Substr2Method() : Func("substr") {}

        Value call(Value self, const vector<Value>& args) {
            string& the_string = self.as<String>();
            Value   the_pos = Reference::deref(args[0]);
            return Value(the_string.substr(the_pos.as<Int>()));
        }
    };

    _add_method((new Function(this))->add_params(Int::self), new Substr2Method());

    // find(str)
    struct FindMethod1 : public Func {
        FindMethod1() : Func("find") {}

        Value call(Value self, const vector<Value>& args) {
            string& the_string = self.as<String>();
            Value   the_searched = Reference::deref(args[0]);
            return Value(int(the_string.find(the_searched.as<String>())));
        }
    };

    _add_method((new Function(Int::self))->add_params(this), new FindMethod1());

    // find(char)
    struct FindMethod2 : public Func {
        FindMethod2() : Func("find") {}

        Value call(Value self, const vector<Value>& args) {
            string& the_string = self.as<String>();
            Value   the_search = Reference::deref(args[0]);
            return Value(int(the_string.find(the_search.as<Char>())));
        }
    };

    _add_method((new Function(Int::self))->add_params(Char::self), new FindMethod2());

    // find(str, pos)
    struct FindMethod3 : public Func {
        FindMethod3() : Func("find") {}

        Value call(Value self, const vector<Value>& args) {
            string& the_string = self.as<String>();
            Value   the_search = Reference::deref(args[0]);
            Value   the_pos = Reference::deref(args[1]);
            return Value(int(the_string.find(the_search.as<String>(), the_pos.as<Int>())));
        }
    };

    _add_method((new Function(Int::self))->add_params(this, Int::self), new FindMethod3());

    // find(char, pos)
    struct FindMethod4 : public Func {
        FindMethod4() : Func("find") {}

        Value call(Value self, const vector<Value>& args) {
            string& the_string = self.as<String>();
            Value   the_char = Reference::deref(args[0]);
            Value   the_pos = Reference::deref(args[1]);
            return Value(int(the_string.find(the_char.as<Char>(), the_pos.as<Int>())));
        }
    };

    _add_method((new Function(Int::self))->add_params(Char::self, Int::self), new FindMethod4());

    // insert(pos, str)
    struct InsertMethod : public Func {
        InsertMethod() : Func("insert") {}

        Value call(Value self, const vector<Value>& args) {
            string& the_string = self.as<String>();
            Value   the_pos = Reference::deref(args[0]);
            Value   the_insert = Reference::deref(args[1]);
            return Value(the_string.insert(the_pos.as<Int>(), the_insert.as<String>()));
        }
    };

    _add_method((new Function(this))->add_params(Int::self, this), new InsertMethod());

    // replace(pos, len, str)
    struct ReplaceMethod : public Func {
        ReplaceMethod() : Func("replace") {}

        Value call(Value self, const vector<Value>& args) {
            string& the_string = self.as<String>();
            Value   the_pos = Reference::deref(args[0]);
            Value   the_len = Reference::deref(args[1]);
            Value   the_replacement = Reference::deref(args[2]);
            return Value(the_string.replace(
                the_pos.as<Int>(), the_len.as<Int>(), the_replacement.as<String>()
            ));
        }
    };

    _add_method((new Function(this))->add_params(Int::self, Int::self, this), new ReplaceMethod());

    // erase(from)
    struct Erase1Method : public Func {
        Erase1Method() : Func("erase") {}

        Value call(Value self, const vector<Value>& args) {
            string& the_string = self.as<String>();
            Value   the_pos = Reference::deref(args[0]);
            return Value(the_string.erase(the_pos.as<Int>()));
        }
    };

    _add_method((new Function(this))->add_params(Int::self), new Erase1Method());

    // erase(from, size)
    struct Erase2Method : public Func {
        Erase2Method() : Func("erase") {}

        Value call(Value self, const vector<Value>& args) {
            string& the_string = self.as<String>();
            Value   the_pos = Reference::deref(args[0]);
            Value   the_size = Reference::deref(args[1]);
            return Value(the_string.erase(the_pos.as<Int>(), the_size.as<Int>()));
        }
    };

    _add_method((new Function(this))->add_params(Int::self, Int::self), new Erase2Method());

    // +
    struct PlusOperator : public Func {
        PlusOperator() : Func("+") {}

        Value call(Value self, const vector<Value>& args) {
            string& the_string = self.as<String>();
            Value   the_other_string = Reference::deref(args[0]);
            return Value(the_string + the_other_string.as<String>());
        }
    };

    _add_method((new Function(this))->add_params(this), new PlusOperator());

    // TODO: +=
    // []
    struct IndexedAccessOperator : public Func {
        IndexedAccessOperator() : Func("[]") {}

        Value call(Value self, const vector<Value>& args) {
            string& the_string = self.as<String>();
            Value   the_index = Reference::deref(args[0]);
            int     k = the_index.as<Int>();
            if (k < 0 or k >= the_string.size()) {
                throw Error("Acceso fuera de rango");  // FIXME
            }
            Value the_char(Char::self_ref, (void *)(&the_string[k]));
            return Reference::mkref(the_char);
        }
    };

    _add_method((new Function(this))->add_params(Int::self), new IndexedAccessOperator());
    // npos
    _add_static("npos", Value(int(string::npos)));
}

template <class C>
Iterator<C>::Iterator(const C *type)
    : Class<BaseType<typename C::cpp_iterator>>("iterator"), _container_type(type) {
    typedef Class<BaseType<typename C::cpp_iterator>> _Class;  // shut up, clang...
                                                               // *

    struct DerefOperator : public Func {
        const C *type;

        DerefOperator(const C *t) : Func("*"), type(t) {}

        Value call(Value self, const vector<Value>& args) {
            typename C::cpp_iterator& the_iterator = self.as<Iterator<C>>();
            Value                     v = C::elem_to_value(type, *the_iterator);
            return Reference::mkref(v);
        }
    };

    _Class::_add_method(new Function(type->celltype()), new DerefOperator(type));
}

template <class C>
string Iterator<C>::to_json(void *data) const {
    typename C::cpp_iterator& the_iterator = *(typename C::cpp_iterator *)data;
    ostringstream             json;
    json << "{\"<type>\":\"iterator\",\"<address>\":\"" << &(*the_iterator) << "\"}";
    return json.str();
}

template <class C>
ForwardIterator<C>::ForwardIterator(const C *type) : Iterator<C>(type) {
    typedef Class<BaseType<typename C::cpp_iterator>> _Class;  // shut up, clang...
                                                               // ++

    struct IncrOperator : public Func {
        IncrOperator() : Func("++") {}

        Value call(Value self, const vector<Value>& args) {
            typename C::cpp_iterator& the_iterator = self.as<Iterator<C>>();
            the_iterator++;
            return self.clone();
        }
    };

    _Class::_add_method(new Function(this), new IncrOperator());
}

template <class C>
BidirectionalIterator<C>::BidirectionalIterator(const C *type) : ForwardIterator<C>(type) {
    typedef Class<BaseType<typename C::cpp_iterator>> _Class;  // shut up, clang...
                                                               // --

    struct DecrOperator : public Func {
        DecrOperator() : Func("--") {}

        Value call(Value self, const vector<Value>& args) {
            typename C::cpp_iterator& the_iterator = self.as<Iterator<C>>();
            the_iterator--;
            return self.clone();
        }
    };

    _Class::_add_method(new Function(this), new DecrOperator());
}

template <class C>
RandomAccessIterator<C>::RandomAccessIterator(const C *type) : BidirectionalIterator<C>(type) {
    typedef Class<BaseType<typename C::cpp_iterator>> _Class;  // shut up, clang...
                                                               // +

    struct PlusOperator : public Func {
        PlusOperator() : Func("+") {}

        Value call(Value self, const vector<Value>& args) {
            typedef typename C::cpp_iterator iter;
            iter&                            the_iterator = self.as<Iterator<C>>();
            Value                            the_int = Reference::deref(args[0]);
            iter                            *sum = new iter(the_iterator + the_int.as<Int>());
            return Value(self.type(), sum);
        }
    };

    _Class::_add_method((new Function(this))->add_params(Int::self), new PlusOperator());
}

string Environment::to_json() const {
    ostringstream json;
    json << "{\"name\":\"" << _name << "\",\"tab\":";
    json << "{\"<active>\":" << (_active ? "true" : "false");
    for (int i = 0; i < _tab.tab.size(); i++) {
        if (_tab.tab[i].has_flag(Hidden)) {
            continue;
        }
        string name = _tab.tab[i].name();
        json << ",\"" << name << "\":";
        json << _tab.tab[i].value().to_json();
    }
    json << "}}";
    return json.str();
}

void Environment::register_type(string name, const Type *type) {
    _curr_namespace.register_type(name, type);
}

const Type *Environment::get_type(TypeSpec *spec, Environment *topmost) {
    if (topmost == 0) {
        topmost = this;
    }
    const Type *type = _curr_namespace.get_type(spec, topmost);
    if (type != 0) {
        return type;
    }
    for (Environment *e : _other_namespaces) {
        type = e->get_type(spec, topmost);
        if (type != 0) {
            return type;
        }
    }
    if (_parent != 0) {
        return _parent->get_type(spec, topmost);
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

Environment *Environment::pop() {
    Environment *parent = _parent;
    delete this;
    return parent;
}

Value OverloadedValue::resolve(const vector<Value>& args) {
    vector<Value>        results;
    list<pair<int, int>> scores;
    for (int i = 0; i < _candidates.size(); i++) {
        const Function *ftype = _candidates[i].type()->as<Function>();
        assert(ftype != 0);
        int score = ftype->check_signature(args);
        if (score != -1) {
            int curr = results.size();
            results.push_back(_candidates[i]);
            scores.push_back(make_pair(score, curr));
        }
    }
    if (results.empty()) {
        _error(_T("No method applicable"));
    }
    Value winner;
    if (results.size() > 1) {
        scores.sort();  // sort by score
        winner = results[scores.back().second];
        // _error(_T("More than one method is applicable"));
    } else {
        winner = results[0];
    }
    return Callable::self->mkvalue(_self, winner);
}

Value Overloaded::mkvalue(Value self, const vector<Value>& candidates) const {
    assert(candidates.size() > 0);
    if (candidates.size() == 1) {
        return candidates[0];
    }
    OverloadedValue *ov = new OverloadedValue();
    ov->_self = self;
    ov->_candidates = candidates;
    return Value(Overloaded::self, ov);
}

int Function::check_signature(const std::vector<Value>& args) const {
    if (args.size() != _param_types.size()) {
        return -1;
    }
    int score = 0;
    for (int i = 0; i < args.size(); i++) {
        // En esta compración de tipos hay que ir con cuidado con
        // Char_ref! Por eso se usa typestr en vez de comparar los punteros
        // directamente
        //
        auto tyParam = _param_types[i]->TypeStr();
        auto tyArg = args[i].type()->TypeStr();
        if (tyParam == tyArg) {
            score++;
        } else if (args[i].type()->is<Reference>()) {
            // If the second type is a reference, try to dereference it
            auto tyArgDeref = args[i].type()->as<Reference>()->subtype()->TypeStr();
            if (tyParam == tyArgDeref) {
                score++;
            }
        } else if (!_param_types[i]->accepts(args[i].type())) {
            return -1;
        }
    }
    return score;
}

void OStream::_add_ostream_methods() {
    // <<
    struct OutputOperator : public Func {
        OutputOperator() : Func("<<") {}

        Value call(Value self, const vector<Value>& args) {
            ostream& out = self.as<OStream>();
            out << args[0];
            return self;
        }
    };

    Func                       *output_op = new OutputOperator();
    static vector<const Type *> BasicTypes = {
        Int::self,
        Char::self,
        Bool::self,
        Float::self,
        Double::self,
        String::self /* FIXME: move to String */
    };
    for (int i = 0; i < BasicTypes.size(); i++) {
        _add_method((new Function(this))->add_params(BasicTypes[i]), output_op);
    }
}

void IStream::_add_istream_methods() {
    // >>
    struct InputOperator : public Func {
        InputOperator() : Func(">>") {}

        Value call(Value self, const vector<Value>& args) {
            istream& in = self.as<IStream>();
            Value    holder = args[0];
            in >> holder;
            return self;
        }

        bool call_abstract(AstNode *x, Value self, const vector<Value>& args) {
            Value holder = args[0];
            if (holder.is_unknown()) {
                holder.to_abstract();
            }
            return true;
        }
    };

    Func                       *input_op = new InputOperator();
    static vector<const Type *> BasicTypes = {
        Int::self,
        Char::self,
        Bool::self,
        Float::self,
        Double::self,
        String::self /* FIXME: move to String */
    };
    for (int i = 0; i < BasicTypes.size(); i++) {
        _add_method((new Function(this))->add_params(BasicTypes[i]), input_op);
    }

    // bool
    struct BoolOperator : public Func {
        BoolOperator() : Func("bool") {}

        Value call(Value self, const vector<Value>& args) {
            return Value(bool(self.as<IStream>()));
        }
    };

    _add_method((new Function(Bool::self)), new BoolOperator());
}

IStringStream::IStringStream() : IStream("istringstream") {
    struct Constructor1 : public Func {
        Constructor1() : Func("istringstream") {}

        Value call(Value self, const vector<Value>& args) {
            istringstream& the_stream = self.as<IStringStream>();
            Value          the_content = Reference::deref(args[0]);
            the_stream.str(the_content.as<String>());
            return Value::null;
        }
    };

    _add_method((new Function(Void))->add_params(String::self), new Constructor1());
}

OStringStream::OStringStream() : OStream("ostringstream") {
    struct StrMethod : public Func {
        StrMethod() : Func("str") {}

        Value call(Value self, const vector<Value>& args) {
            ostringstream& the_stream = self.as<OStringStream>();
            return Value(the_stream.str());
        }
    };

    _add_method(new Function(String::self), new StrMethod());
}

struct MaxFunc : public Func {
    MaxFunc() : Func("max") {}

    Value call(Value self, const vector<Value>& args) {
        assert(args.size() == 2);
        assert(args[0].is<Int>());
        assert(args[1].is<Int>());
        return Value(std::max(args[0].as<Int>(), args[1].as<Int>()));
    }
};

MaxFunc _max;

struct TojsonFunc : public Func {
    TojsonFunc() : Func("to_json") {}

    Value call(Value self, const vector<Value>& args) {
        assert(args.size() == 1);
        Value arg0 = Reference::deref(args[0]);
        return Value(arg0.to_json());
    }
};

TojsonFunc _tojson;

struct GetlineFunc : public Func {
    GetlineFunc() : Func("getline") {}

    Value call(Value self, const vector<Value>& args) {
        Value the_string = Reference::deref(args[1]);
        Value the_istream = Reference::deref(args[0]);
        return Value(std::getline(the_istream.as<IStream>(), the_string.as<String>()));
    }

    Value mkcallable() {
        Function *functype = (new Function(IStream::self))->add_params(IStream::self, String::self);
        return Callable::self->mkvalue(Value::null, functype->mkvalue(this));
    }
};

GetlineFunc _getline;
const bool  hidden = true;

void WithEnvironment::prepare_global_environment() {
    _namespaces.clear();
    Environment *cpp = new Environment("[c++]", 0, hidden);  // for C++ integrated stuff
    cpp->register_type("int", Int::self);
    cpp->register_type("char", Char::self);
    cpp->register_type("float", Float::self);
    cpp->register_type("double", Double::self);
    cpp->register_type("bool", Bool::self);
    Environment *global = new Environment("[global]", cpp, hidden);
    Environment *std = new Environment("std", cpp, hidden);
    Environment *minicc = new Environment("minicc", cpp, hidden);
    _namespaces["[c++]"] = cpp;
    _namespaces["[global]"] = global;
    _namespaces["std"] = std;
    _namespaces["minicc"] = minicc;
    _env = global;
}

const Type *WithEnvironment::get_type(TypeSpec *spec) {
    Identifier *namespc = spec->GetPotentialNamespaceOrClass();
    if (namespc != 0) {
        auto it = _namespaces.find(namespc->name);
        if (it != _namespaces.end()) {
            namespc->is_namespace = true;
            Environment *e = it->second;
            return e->get_type(spec);
        }
    }
    return _env->get_type(spec);
}

void WithEnvironment::register_type(string name, const Type *type) {
    _env->register_type(name, type);
}

void WithEnvironment::setenv(string id, Value v, int flags) {
    _env->set(id, v, flags);
}

bool WithEnvironment::getenv(string id, Value& v) {
    return _env->get(id, v);
}

bool WithEnvironment::has_flag(string id, Flag f) const {
    return _env->has_flag(id, f);
}

bool WithEnvironment::using_namespace(string name) {
    auto it = _namespaces.find(name);
    if (it == _namespaces.end()) {
        return false;
    }
    _env->using_namespace(it->second);
    return true;
}

Environment *WithEnvironment::get_namespace(string name) {
    auto it = _namespaces.find(name);
    return (it == _namespaces.end() ? 0 : it->second);
}

bool WithEnvironment::include_header_file(string name) {
    Environment *std = _namespaces["std"];
    Environment *minicc = _namespaces["minicc"];
    if (name == "iostream") {
        std->register_type("ostream", OStream::self);
        std->register_type("istream", IStream::self);
        std->register_type("string", String::self);  // 'iostream' includes 'string'
        std->set("endl", Value("\n"), hidden);
        // std->set("cerr", Cerr, hidden);
        std->set("cout", Value(*_out), hidden);
        std->set("cin", Value(*_in), hidden);
        std->set("getline", _getline.mkcallable(), hidden);
    } else if (name == "vector") {
        std->register_type("vector", Vector::self);
        std->register_type("string", String::self);  // 'vector' includes 'string'
    } else if (name == "list") {
        std->register_type("list", List::self);
    } else if (name == "map") {
        std->register_type("pair", Pair::self);
        std->register_type("map", Map::self);
    } else if (name == "string") {
        std->register_type("string", String::self);
    } else if (name == "sstream") {
        std->register_type("istringstream", IStringStream::self);
        std->register_type("ostringstream", OStringStream::self);
    } else if (name == "algorithm") {
        // FIXME: make max a template, so that when you call it,
        // it instantiates the proper type of function.
        //
        Function *max_func_type = new Function(Int::self);
        max_func_type->add_params(Int::self, Int::self);
        Value func = max_func_type->mkvalue(&_max);
        Value callable = Callable::self->mkvalue(Value::null, func);
        std->set("max", callable);
    } else if (name == "json") {
        Function *tojson_func_type = new Function(String::self);
        tojson_func_type->add_params(Any);
        Value func = tojson_func_type->mkvalue(&_tojson);
        Value callable = Callable::self->mkvalue(Value::null, func);
        minicc->set("tojson", callable);
    } else {
        return false;
    }
    return true;
}

string WithEnvironment::env2json() const {
    Environment  *e = _env;
    ostringstream json;
    json << "[";
    int i = 0;
    while (e != 0) {
        if (!e->hidden()) {
            if (i > 0) {
                json << ",";
            }
            json << e->to_json();
            i++;
        }
        e = e->parent();
    }
    json << "]";
    return json.str();
}
