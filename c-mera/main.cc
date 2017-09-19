
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include "file.cc"
#include "lexer.cc"

int main(int argc, char **argv) {
   init();
   if (argc < 2) {
      fprintf(stderr, "usage: minicc <file>\n");
      exit(1);
   }
   char *filename = argv[1];
   const char *buffer;
   buffer = read_whole_file(filename);
   Token tok;
   Lexer lexer;
   lexer.init(buffer);
   while (1) {
      tok = lexer.get();
      if (tok.atom->kind == END) {
         break;
      }
      printf("%s \"%.*s\" %lu\n",
             kind2str(tok.atom->kind),
             (int) tok.atom->len,
             tok.atom->str,
             tok.atom->len);
   }
}