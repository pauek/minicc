#ifndef VALUE_HH
#define VALUE_HH

#include <cstring>
#include "ast.hh"

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
               Array, Struct, Ref,
               Cout, Cin, Cerr,
               Unknown };

   Any         val;
   Kind        kind;
   std::string type; // string representation of the type (as returned by Type::str)

   void _clear() { std::memset(&val, 0, sizeof(Any)); }

   Value() : kind(Unknown) { _clear(); }
   Value(const Value& v);
   Value(Kind k) : kind(k) { _clear(); }
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

   Value operator=(const Value& v);

   static Value cout, cin, cerr;
};

bool operator==(const Value& a, const Value& b);
std::ostream& operator<<(std::ostream& o, const Value& v);

#endif

