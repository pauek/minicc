/////////////////////////////////////////////////////////////////////////////////////////////
#if defined(DECLARATION)

#include <stdio.h>

char *read_whole_file(const char *filename);

#endif // DECLARATION
/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
#if defined(IMPLEMENTATION)


char *read_whole_file(char const *filename) {
   FILE *F = fopen(filename, "r");
   if (F == 0) {
      // fprintf(stderr, "Can't open '%s'\n", filename);
      return 0;
   }
   fseek(F, 0, SEEK_END);
   size_t size = (size_t)ftell(F);
   char *buffer = (char*)malloc(size + 1);
   if (buffer == 0) {
      fprintf(stderr, "Cannot allocate buffer\n");
      return 0;
   }
   fseek(F, 0, SEEK_SET);
   size_t n_bytes_read = fread((void*)buffer, 1, size, F);
#if defined(WINDOWS)
   // NOTE(pauek): MSVC's 'fread' does change the number of bytes in text files
   // due to substitution of "\r\n" by "\n", so the count doesn't agree.
   if (ferror(F) != 0) {
      fprintf(stderr, "Could't read whole file\n");
      free(buffer);
      return 0;
   }
#else
   if (n_bytes_read != (size_t)size) {
      printf("%lu %ld\n", n_bytes_read, size);
      fprintf(stderr, "Could't read whole file\n");
      free(buffer);
      return 0;
   }
#endif
   buffer[n_bytes_read] = 0;
   return buffer;
}


#endif // IMPLEMENTATION
/////////////////////////////////////////////////////////////////////////////////////////////
