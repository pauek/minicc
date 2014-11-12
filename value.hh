#ifndef VALUE_HH
#define VALUE_HH

#include <cstring>
#include "ast.hh"

struct refcounted {
   int count;
};

struct Type;
class Value_ { // new value
   struct Box {
      int         count;
      const Type *type;
      void       *data;
      Box(const Type *t, void *d) : count(0), type(t), data(d) {}
      Box() : count(0), type(0), data(0) {}
   };
   Box *_box;

   void _detach(Box *b);
   void _attach(Box *b);

public:
   Value_(const Type *t = 0, void *d = 0);

   Value_(int x);
   Value_(char x);
   Value_(bool x);
   Value_(float x);
   Value_(double x);
   Value_(const char *x); // string!

   const Type *type() const { return _box->type; }

   template<typename T> bool is() const;
   template<typename T> typename T::cpp_type as() const;
   bool is(Type *t) const { return _box->type == t; }

   ~Value_();

   Value_(const Value_& v);
   const Value_& operator=(const Value_& v);

   friend class Reference;
};

struct Value 
{
   union Any {
      bool   as_bool;
      char   as_char;
      int    as_int;
      float  as_float;
      double as_double;
      void  *as_ptr;      
   };

   enum Kind { Bool, Char, Int, Float, Double,
               String, Vector, List, Map,
               ExprList, Array, Struct, Ref,
               Cout, Cin, Cerr,
               Unknown };

   Any         val;
   Kind        kind;
   std::string type; // string representation of the type (as returned by Type::str)
   bool        hidden;

   void _clear() { std::memset(&val, 0, sizeof(Any)); }

   Value()                      : kind(Unknown)    { _clear(); }
   Value(Kind k)                : kind(k)          { _clear(); }
   Value(Kind k, std::string t) : kind(k), type(t) { _clear(); }

   Value(const Value& v);
   ~Value();

   Value(bool x)        : type("bool"),    kind(Bool)   { _clear(); val.as_bool = x; }
   Value(char x)        : type("char"),    kind(Char)   { _clear(); val.as_char = x; }
   Value(int x)         : type("int"),     kind(Int)    { _clear(); val.as_int = x; }
   Value(float x)       : type("float"),   kind(Float)  { _clear(); val.as_float = x; }
   Value(double x)      : type("double"),  kind(Double) { _clear(); val.as_double = x; }
   Value(std::string s) : type("string"),  kind(String) { _clear(); val.as_ptr = new std::string(s); }
   Value(const char *s) : type("string"),  kind(String) { _clear(); val.as_ptr = new std::string(s); }

   template<class T>
   T ref_to() const {
      if (kind != Ref) {
         return 0;
      }
      return static_cast<T>(val.as_ptr);
   }

   Value *ref() const { return ref_to<Value*>(); }
   Value operator=(const Value& v);

   std::string to_json() const;

   static Value cout, cin, cerr;

   static Kind type2kind(std::string type);

   std::vector<Value*> *exprlist();
   const std::vector<Value*> *exprlist() const;
};

bool operator==(const Value& a, const Value& b);
std::ostream& operator<<(std::ostream& o, const Value& v);
std::istream& operator>>(std::istream& o, Value& v);

inline bool operator!=(const Value& a, const Value& b) {
   return !operator==(a, b);
}

struct Environment {
   struct Item {
      std::string  name;
      Value       *value;
      bool         hidden;
      Item(std::string n, Value *v, bool h = false)
         : name(n), value(v), hidden(h) {}
   };

   bool              active;
   std::string       name;
   std::vector<Item> tab;

   Item *_get(std::string name);

public:
   Environment(std::string n) : name(n), active(false) {}

   void set(std::string name, Value *v, bool hidden = false) {
      Item *i = _get(name);
      if (i == 0) {
         tab.push_back(Item(name, v, hidden));
      } else {
         i->value = v;
      }
   }

   Value *get(std::string name) {
      Item *i = _get(name);
      return (i ? i->value : 0);
   }

   void to_json(std::ostream& o) const;
   std::map<std::string,Value*> *as_map() const;
};

std::string json_encode(std::string s);

#endif

