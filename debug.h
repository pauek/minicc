#if defined(DEBUG) 
/////////////////////////////////////////////////////////////////////////////////////////////
#if defined(DECLARATION)


#include <stdio.h>

char *lexer_token_kind(TokenKind kind);
void  print_all_atoms();


#endif // DECLARATION
/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
#if defined(IMPLEMENTATION)


void print_all_atoms() {
   for (int i = 0; i < ATOM_NUM_NODES; i++) {
      if (nodes[i]) {
         printf("%d:", i);
         for (Node *n = nodes[i]; n; n = n->prev) {
            printf(" %s", n->atom.str);
         }
         printf("\n");
      }
   }
}

char *lexer_token_kind(TokenKind kind) {
   static char buffer[32];
   switch (kind) {
   case TOK_EOF:        sprintf(buffer, "EOF"); break;
   case TOK_ERROR:      sprintf(buffer, "ERROR"); break;
   case TOK_OPERATOR:   sprintf(buffer, "OPERATOR"); break;
   case TOK_PUNCT:      sprintf(buffer, "PUNCT"); break;
   case TOK_DELIM:      sprintf(buffer, "DELIM"); break;
   case TOK_IDENT:      sprintf(buffer, "IDENT"); break;
   case TOK_FILENAME:   sprintf(buffer, "FILENAME"); break;
   case TOK_CONTROL:    sprintf(buffer, "CONTROL"); break;
   case TOK_DIRECTIVE:  sprintf(buffer, "DIRECTIVE"); break;
   case TOK_TYPE:       sprintf(buffer, "TYPE"); break;
   case TOK_TYPEDEF:    sprintf(buffer, "TYPEDEF"); break;
   case TOK_MODIFIER:   sprintf(buffer, "MODIFIER"); break;
   case TOK_USING:      sprintf(buffer, "USING"); break;
   case TOK_LIT_INT:    sprintf(buffer, "LIT_INT"); break;
   case TOK_LIT_FLOAT:  sprintf(buffer, "LIT_FLOAT"); break;
   case TOK_LIT_DOUBLE: sprintf(buffer, "LIT_DOUBLE"); break;
   case TOK_LIT_BOOL:   sprintf(buffer, "LIT_BOOL"); break;
   case TOK_LIT_STRING: sprintf(buffer, "LIT_STRING"); break;
   case TOK_LIT_CHAR:   sprintf(buffer, "LIT_CHAR"); break;
   case TOK_BACKSLASH:  sprintf(buffer, "BACKSLASH"); break;
   }
   return buffer;
}


#endif // IMPLEMENTATION
/////////////////////////////////////////////////////////////////////////////////////////////
#endif
