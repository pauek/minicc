
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>

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
      Pos pos = lexer.position(tok.pos);
      printf("%d:%d: %s \"%.*s\" %lu\n",
             pos.lin, pos.col,
             kind2str(tok.atom->kind),
             (int) tok.atom->len,
             tok.atom->str,
             tok.atom->len);
      if (tok.atom->kind == ERROR) {
         printf("Error, exiting!\n");
         break;
      }
      if (tok.atom->kind == END) {
         break;
      }
   }
}
