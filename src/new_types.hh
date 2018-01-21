
#ifndef NEW_TYPES_HH
#define NEW_TYPES_HH

#include <cstdint>
#include <cassert>
#include <string>
#include <vector>

namespace vm {

typedef uint16_t type_index_t;
typedef size_t   name_index_t;

enum TypeTag : uint8_t { I32, U32, F32, I64, U64, F64, Composite };
enum TypeDescTag : uint8_t { Struct, Array };

struct Type {  // This has a constant size (4 bytes), which is important
   TypeTag      tag;
   type_index_t index; // index in the type description table

   // TODO: Maybe use more space for the index? There is one spare byte...

   Type(TypeTag _tag, type_index_t _index = 0) {
      tag   = _tag;
      index = _index;
   }
};

struct TypeDescription { // The layout description of a compound type
   name_index_t name;    // index to the name table
   TypeDescTag Tag() const { return _tag; }
protected:
   TypeDescTag _tag;
};

template<TypeDescTag T>
struct TypeDescriptionDerived : TypeDescription {
   static bool is_instance(const TypeDescription *desc) { return desc->Tag() == T; }
   TypeDescriptionDerived() { _tag = T; }
};

struct Field {
   name_index_t name;
   Type    type;
};

struct StructDescription : TypeDescriptionDerived<Struct> {
   std::vector<Field> fields;
   StructDescription(name_index_t _name, const std::vector<Field>& _fields) {
      name   = _name;
      fields = _fields;
   }
};
struct ArrayDescription : TypeDescriptionDerived<Array> {
   size_t size;
   Type   elem_type;
   ArrayDescription(name_index_t _name, size_t _size, Type _elem_type) 
      : size(_size), elem_type(_elem_type) { name = _name; }
};

class TypeTable {
   std::vector<TypeDescription*> _types;
public:
   TypeDescription *Get(type_index_t index) {
      assert(index >= 0 && index < _types.size());
      return _types[(size_t)index];
   }

   size_t SizeOf(Type type);
   type_index_t Add(TypeDescription *desc);
};

}

#endif // NEW_TYPES_HH