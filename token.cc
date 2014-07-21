
#include <iostream>
#include "token.hh"
using namespace std;

// OJO: El orden de la tabla es importante!
// Hay que dejarla antes que el _table...
//
struct { string tok; Token::Type type; } toktab[] = {
   { "{",        Token::LCurly },
   { "if",       Token::If },
   { "else",     Token::Else },
   { "while",    Token::While },
   { "for",      Token::For },
   { "switch",   Token::Switch },
   { "break",    Token::Break },
   { "continue", Token::Continue },
   { "using",    Token::Using },
   { "struct",   Token::Struct },
   { "class",    Token::Class },
   { "typedef",  Token::Typedef },
   { "goto",     Token::Goto },

   { "END",      Token::Unknown }
};

Token::Table Token::_table;

Token::Table::Table() {
   int i = 0;
   while (toktab[i].tok != "END") {
      _table._map[toktab[i].tok] = toktab[i].type;
      i++;
   }
}

Token::Type Token::token2type(std::string tok) {
   auto it = _table._map.find(tok);
   return (it != _table._map.end() ? it->second : Token::Unknown);
}
