/////////////////////////////////////////////////////////////////////////////////////////////
#if defined(DECLARATION)


#include <stdarg.h>

struct Buffer {
   int   len;
   int   avail;
   char *str;
};

 Buffer *buf_new();
   void  buf_free(Buffer *B);
   void  buf_expand(Buffer *B);
   void  buf_reset(Buffer *B);
   void  buf_printf(Buffer *B, const char *fmt, ...);


#endif // DECLARATION
/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
#if defined(IMPLEMENTATION)


#define BUF_INITIAL_AVAIL 16

Buffer *buf_new() {
   Buffer *result = (Buffer *)malloc(sizeof(Buffer));
   result->len   = 0;
   result->avail = BUF_INITIAL_AVAIL;
   result->str   = (char *)malloc(BUF_INITIAL_AVAIL);
   return result;
}

void buf_free(Buffer *B) {
   if (B->len > 0) {
      free(B->str);
      free(B);
   }
}

void buf_reset(Buffer *B) {
   B->len = 0;
}

void buf_expand(Buffer *B) {
   int newavail = B->avail * 2;
   char *newstr = (char *)malloc(newavail);
   memcpy(newstr, B->str, B->len);
   B->str   = newstr;
   B->avail = newavail;
}

void buf_printf(Buffer *B, const char *fmt, ...) {
    va_list args;
    loop {
        int left = B->avail - B->len;
        va_start(args, fmt);
        int written = vsnprintf(B->str + B->len, left, fmt, args);
        va_end(args);
        if (written >= left) {
            buf_expand(B);
            continue;
        }
        B->len += written;
        B->str[B->len] = 0;
        return;
    }
}


#endif // IMPLEMENTATION
/////////////////////////////////////////////////////////////////////////////////////////////
