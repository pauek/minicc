/////////////////////////////////////////////////////////////////////////////////////////////
#if defined(DECLARATION)

#include <assert.h> // assert
#include <stdlib.h> // malloc, realloc
#include <string.h> // memcpy
#include <stdint.h> // uint8_t

struct Array {
   size_t   len;
   size_t   size;
   uint8_t *data;
};

        Array *array_new(size_t len, size_t size);
         void  array_free(Array *array);
         void *array_get(Array *array, int i);
         void *array_put(Array *array, int i, void *elem);
         void  array_resize(Array *array, size_t len);
        Array *array_copy(Array *array, size_t len);
         void  array_push(Array *array, void *elem);
inline size_t  array_len(Array *array)  { assert(array); return array->len; }
inline size_t  array_size(Array *array) { assert(array); return array->size; }


#endif // DECLARATION
/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
#if defined(IMPLEMENTATION)


Array *array_new(size_t len, size_t size) {
   Array *array;
   array = (Array *)malloc(sizeof(Array));
   array->len  = len;
   array->size = size;
   array->data = NULL;
   if (len > 0) {
      array->data = (uint8_t *)calloc(len, size);
   } 
   return array;
}

void array_free(Array *array) {
   assert(array);
   if (array->data != NULL) {
      free(array->data);
   }
   free(array);
}

void *array_get(Array *array, int i) {
   assert(array);
   assert(i >= 0 && i < (int)array->len);
   return array->data + i * array->size;
}

void *array_put(Array *array, int i, void *elem) {
   assert(array);
   assert(i >= 0 && i < (int)array->len);
   assert(elem);
   memcpy(array->data + i * array->size, elem, array->size);
   return elem;
}

void array_resize(Array *array, size_t len) {
   assert(array);
   if (len == 0) {
      if (array->data != NULL) {
         free(array->data);
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

Array *array_copy(Array *array, size_t len) {
   Array *copy;
   assert(array);
   copy = array_new(len, array->size);
   if (copy->len >= array->len && array->len > 0) {
      memcpy(copy->data, array->data, array->len * array->size);
   } else if (array->len > copy->len && copy->len > 0) {
      memcpy(copy->data, array->data, copy->len * array->size);
   }
   return copy;
}

void array_push(Array *array, void *elem) {
   array_resize(array, array->len + 1);
   array_put(array, (int)array->len - 1, elem);
}


#endif // IMPLEMENTATION
/////////////////////////////////////////////////////////////////////////////////////////////
