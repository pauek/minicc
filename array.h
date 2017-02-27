/////////////////////////////////////////////////////////////////////////////////////////////
#if defined(DECLARATION)

#include <assert.h> // assert
#include <stdlib.h> // malloc, realloc
#include <string.h> // memcpy
#include <stdint.h> // uint8_t

namespace array {

struct T {
   size_t   len;
   size_t   size;
   uint8_t *data;
};

            T *make(size_t len, size_t size);
         void  free(T *array);
         void *get(T *array, int i);
         void *put(T *array, int i, void *elem);
         void  resize(T *array, size_t len);
            T *copy(T *array, size_t len);
         void  push(T *array, void *elem);
inline size_t  len(T *array)  { assert(array); return array->len; }
inline size_t  size(T *array) { assert(array); return array->size; }

}

#endif // DECLARATION
/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
#if defined(IMPLEMENTATION)

namespace array {

T *make(size_t len, size_t size) {
   T *array;
   array = (T *)malloc(sizeof(T));
   array->len  = len;
   array->size = size;
   array->data = NULL;
   if (len > 0) {
      array->data = (uint8_t *)calloc(len, size);
   } 
   return array;
}

void free(T *array) {
   assert(array);
   if (array->data != NULL) {
      ::free(array->data);
   }
   ::free(array);
}

void *get(T *array, int i) {
   assert(array);
   assert(i >= 0 && i < (int)array->len);
   return array->data + i * array->size;
}

void *put(T *array, int i, void *elem) {
   assert(array);
   assert(i >= 0 && i < (int)array->len);
   assert(elem);
   memcpy(array->data + i * array->size, elem, array->size);
   return elem;
}

void resize(T *array, size_t len) {
   assert(array);
   if (len == 0) {
      if (array->data != NULL) {
         ::free(array->data);
      }
      array->data = 0;
   } else if (array->len == 0) {
      if (len > 0) {
         array->data = (uint8_t *)malloc(len * array->size);
      }
   } else {
      array->data = (uint8_t *)realloc(array->data, len * array->size);
   }
   array->len = len;
}

T *copy(T *array, size_t len) {
   T *copy;
   assert(array);
   copy = make(len, array->size);
   if (copy->len >= array->len && array->len > 0) {
      memcpy(copy->data, array->data, array->len * array->size);
   } else if (array->len > copy->len && copy->len > 0) {
      memcpy(copy->data, array->data, copy->len * array->size);
   }
   return copy;
}

void push(T *array, void *elem) {
   resize(array, array->len + 1);
   put(array, (int)array->len - 1, elem);
}

}


#endif // IMPLEMENTATION
/////////////////////////////////////////////////////////////////////////////////////////////
