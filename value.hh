#ifndef VALUE_HH
#define VALUE_HH

#include <cstring>
#include "ast.hh"
#include "util.hh"

struct Type;
class Value { // new value
   struct Box {
      int         count;
      const Type *type;
      void       *data;

      Box() : count(0), type(0), data(0) {}
      Box(const Type *t, void *d) : count(0), type(t), data(d) {}
   };
   Box *_box;

   void _detach(Box *b);
   void _attach(Box *b);

   explicit Value(Box *box);

public:
   explicit Value() : _box(0) {}
   explicit Value(const Type *t, void *d);
   Value(const Value& v);

   explicit Value(int x);
   explicit Value(char x);
   explicit Value(bool x);
   explicit Value(float x);
   explicit Value(double x);
   explicit Value(std::string x);
   explicit Value(const char *x); // string!
   explicit Value(std::ostream& o);
   explicit Value(std::istream& i);

   ~Value();

   const Type *type() const { return (_box == 0 ? 0 : _box->type); }

   template<typename T> bool is() const;
   template<typename T> typename T::cpp_type& as() const;
   bool has_type(const Type *t) const { return _box->type == t; }

   static Value null;
   bool is_null() const { return _box == 0; }

   std::string type_name() const;

   bool same_type_as(const Value& v) const { 
      return _box->type == v._box->type; 
   }
   
   // This means "it is the same object" (the same Box), like in Java
   const bool operator==(const Value& v) const {
      return _box == v._box;
   }
   bool equals(const Value& v) const; // Comparison of data

   const Value& operator=(const Value& v); // copies reference, not Box!
   bool assign(const Value& v); // copies content of Box
   Value clone() const;

   void write(std::ostream& o) const;
   void read(std::istream& i);
   std::string to_json() const;

   friend class Reference;
};

std::ostream& operator<<(std::ostream& o, const Value& v);
std::istream& operator>>(std::istream& o, Value& v);


struct Environment : public SimpleTable<Value> {
   std::string name;
   bool        active;
public:
   Environment(std::string n) : name(n), active(false) {}
   std::string to_json() const;
};

std::string json_encode(std::string s);

extern Value Cout, Cin, Cerr, Endl;

#endif

/*

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

inline bool operator!=(const Value& a, const Value& b) {
   return !operator==(a, b);
}

*/
