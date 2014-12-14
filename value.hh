#ifndef VALUE_HH
#define VALUE_HH

#include <cstring>
#include "ast.hh"
#include "util.hh"

struct Type;
class Value { // new value
   struct Box {
      int   count;
      Type *type;
      void *data;

      Box() : count(0), type(0), data(0) {}
      Box(Type *t, void *d) : count(0), type(t), data(d) {}
   };
   Box *_box;

   void _detach(Box *b);
   void _attach(Box *b);

   explicit Value(Box *box);

public:
   explicit Value() : _box(0) {}
   explicit Value(Type *t, void *d);
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

   Type *type() const { return (_box == 0 ? 0 : _box->type); }
   void *data()       { return (_box == 0 ? 0 : _box->data); }

   template<typename T> bool is() const;
   template<typename T> typename T::cpp_type& as();
   template<typename T> typename T::cpp_type& as() const;
   bool has_type(const Type *t) const { return _box->type == t; }

   static Value null;
   bool is_null() const { return _box == 0; }

   std::string type_name() const;

   bool same_type_as(const Value& v) const;
   
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

#endif
