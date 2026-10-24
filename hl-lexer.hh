#ifndef HL_LEXER_HH
#define HL_LEXER_HH

#include <iostream>

#include "lexer.hh"

class HLLexer {
    const Lexer& lexer_;

   public:
    HLLexer(const Lexer& lexer) : lexer_(lexer) {}

    void dump(std::ostream& out);
};

#endif
