
/////////////////////////////////////////////////////////////////////////////////////////////
#if defined(DECLARATION)

enum Type_Tag {
   Type_Void,
   Type_Bool,
   Type_Char,
   Type_Int,
   Type_Float,
   Type_Double,
   Type_Ptr,
   Type_Enum,
   Type_Struct,
   Type_Func,
};

struct Type {
   Type_Tag tag;
   int      size;
   int      align;
   bool     unsignd;

   union {
      // array
      struct {
         int   len;
         Type *subtype;
      };
       // pointer or reference
      Type *ptr;
      // struct
      struct {
         Array *fields;
      };
      // function 
      struct {
         Type  *rettype;
         Array *params;
      };
   };
};

#define TYPE(name, tag, size, align, unsignd) \
   extern Type *type_##name;
#include "type.inc"
#undef  TYPE

#endif // DECLARATION
/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
#if defined(IMPLEMENTATION)

Type *type_make(Type T) {
   Type *type = (Type *)malloc(sizeof(Type));
   *type = T;
   return type;
}

Type *type_make_ptr(Type *ptr) {
   Type *T = type_make((Type){Type_Ptr, 8, 8});
   T->ptr = ptr;
   return T;
}

#define TYPE(name, tag, size, align, unsignd) \
   Type *type_##name = type_make((Type){tag, size, align, unsignd});
#include "type.inc"
#undef  TYPE

#endif // IMPLEMENTATION
/////////////////////////////////////////////////////////////////////////////////////////////
