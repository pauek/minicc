
#include "cast.h"
#include "new_types.hh"

namespace vm {

size_t TypeTable::SizeOf(Type type) {
   switch (type.tag) {
   case I32: case U32: case F32:
      return 4;
   case I64: case U64: case F64:
      return 8;
   case Composite: {
      TypeDescription *_desc = Get(type.index);
      switch (_desc->Tag()) {
      case Struct: {
         StructDescription *struc = cast<StructDescription>(_desc);
         size_t size = 0;
         for (int i = 0; i < struc->fields.size(); i++) {
            size += SizeOf(struc->fields[i].type);
         }
         return size;
      }
      case Array: {
         ArrayDescription *array = cast<ArrayDescription>(_desc);
         return array->size * SizeOf(array->elem_type);
      }
      }
   }
   }
}

type_index_t TypeTable::Add(TypeDescription *desc) {
   _types.push_back(desc);
   return _types.size()-1;
}

}