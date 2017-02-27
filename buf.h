/////////////////////////////////////////////////////////////////////////////////////////////
#if defined(DECLARATION)


#include <stdarg.h>

namespace buf {

struct T {
   int   len;
   int   avail;
   char *str;
};

      T *make();
   void  free(T *B);
   void  expand(T *B);
   void  reset(T *B);
   void  printf(T *B, const char *fmt, ...);

} // namespace buf

typedef buf::T Buffer;


#endif // DECLARATION
/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
#if defined(IMPLEMENTATION)


#define BUF_INITIAL_AVAIL 16

namespace buf {

T *make() {
   T *result = (T *)malloc(sizeof(T));
   result->len   = 0;
   result->avail = BUF_INITIAL_AVAIL;
   result->str   = (char *)malloc(BUF_INITIAL_AVAIL);
   return result;
}

void free(T *B) {
   if (B->len > 0) {
      ::free(B->str);
      ::free(B);
   }
}

void reset(T *B) {
   B->len = 0;
}

void expand(T *B) {
   int newavail = B->avail * 2;
   char *newstr = (char *)malloc(newavail);
   memcpy(newstr, B->str, B->len);
   B->str   = newstr;
   B->avail = newavail;
}

void printf(T *B, const char *fmt, ...) {
    va_list args;
    loop {
        int left = B->avail - B->len;
        va_start(args, fmt);
        int written = vsnprintf(B->str + B->len, left, fmt, args);
        va_end(args);
        if (written >= left) {
            expand(B);
            continue;
        }
        B->len += written;
        B->str[B->len] = 0;
        return;
    }
}

} // namespace buf


#endif // IMPLEMENTATION
/////////////////////////////////////////////////////////////////////////////////////////////
