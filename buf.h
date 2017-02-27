/////////////////////////////////////////////////////////////////////////////////////////////
#if defined(DECLARATION)


#include <stdarg.h>

namespace buf {

struct Buffer {
   int   len;
   int   avail;
   char *str;
};

 Buffer *make();
   void  free(Buffer *B);
   void  expand(Buffer *B);
   void  reset(Buffer *B);
   void  printf(Buffer *B, const char *fmt, ...);

} // namespace buf

#endif // DECLARATION
/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
#if defined(IMPLEMENTATION)


#define BUF_INITIAL_AVAIL 16

namespace buf {

Buffer *make() {
   Buffer *result = (Buffer *)malloc(sizeof(Buffer));
   result->len   = 0;
   result->avail = BUF_INITIAL_AVAIL;
   result->str   = (char *)malloc(BUF_INITIAL_AVAIL);
   return result;
}

void free(Buffer *B) {
   if (B->len > 0) {
      ::free(B->str);
      ::free(B);
   }
}

void reset(Buffer *B) {
   B->len = 0;
}

void expand(Buffer *B) {
   int newavail = B->avail * 2;
   char *newstr = (char *)malloc(newavail);
   memcpy(newstr, B->str, B->len);
   B->str   = newstr;
   B->avail = newavail;
}

void printf(Buffer *B, const char *fmt, ...) {
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
