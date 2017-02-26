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

#endif // DECLARATION
/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
#if defined(IMPLEMENTATION)

Type *type_new(Type T) {
   Type *type = (Type *)malloc(sizeof(Type));
   *type = T;
   return type;
}

Type *type_void   = type_new((Type){Type_Void,   0, 0});
Type *type_bool   = type_new((Type){Type_Bool,   1, 1});
Type *type_char   = type_new((Type){Type_Char,   1, 1});
Type *type_int    = type_new((Type){Type_Int,    4, 4});
Type *type_uchar  = type_new((Type){Type_Char,   1, 1, true});
Type *type_uint   = type_new((Type){Type_Int,    4, 4, true});
Type *type_float  = type_new((Type){Type_Float,  4, 4});
Type *type_double = type_new((Type){Type_Double, 8, 8});
Type *type_enum   = type_new((Type){Type_Enum,   4, 4});


#endif // IMPLEMENTATION
/////////////////////////////////////////////////////////////////////////////////////////////
