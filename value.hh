#ifndef VALUE_HH
#define VALUE_HH

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
               Unknown };
   Any   val;
   Kind  kind;
   Type *type;

   Value(Type *t = 0) : type(t), kind(Unknown) {}
   Value(int x)  : kind(Int)  { val.as_int = x; }
   Value(char c) : kind(Char) { val.as_char = c; }

   template<class T>
   T ref_to() const {
      if (kind != Ref) {
         return 0;
      }
      return static_cast<T>(val.as_ptr);
   }
};

std::ostream& operator<<(std::ostream& o, const Value& v);

#endif

