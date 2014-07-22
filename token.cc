
#include <iostream>
#include "token.hh"
using namespace std;

// OJO: El orden de la tabla es importante!
// Hay que dejarla antes que el _table...
//
struct { 
   string      s; 
   Token::Type t; 
   Token::Kind k;
} toktab[] = {
   { "",           Token::Empty, Token::None },

   { ",",          Token::Comma, Token::None },

   { "||",        Token::BarBar, Token::None },
   { "&&",        Token::AmpAmp, Token::None },
   { "=",         Token::Assign, Token::None },
   { "*=",    Token::StarAssign, Token::None },
   { "-=",   Token::MinusAssign, Token::None },
   { "+=",    Token::PlusAssign, Token::None },
   { "/=",   Token::SlashAssign, Token::None },
   { "%=",     Token::DivAssign, Token::None },
   { "|=",      Token::OrAssign, Token::None },
   { "&=",     Token::AndAssign, Token::None },
   { "^=",     Token::XorAssign, Token::None },
   { "<<=", Token::LShiftAssign, Token::None },
   { ">>=", Token::RShiftAssign, Token::None },
   { "or",            Token::Or, Token::None },
   { "and",          Token::And, Token::None },

   { "==",          Token::EqEq, Token::None },
   { "!=",         Token::NotEq, Token::None },
   { "<",             Token::LT, Token::None },
   { ">",             Token::GT, Token::None },
   { ">=",            Token::GE, Token::None },
   { "<=",            Token::LE, Token::None },
   { "<<",        Token::LShift, Token::None },
   { ">>",        Token::RShift, Token::None },

   { "#",        Token::Sharp,    Token::None },
   { ".",        Token::Dot,      Token::None },
   { "->",       Token::Arrow,    Token::None },
   { "{",        Token::LCurly,   Token::None },
   { "[",        Token::LBrack,   Token::None },
   { "(",        Token::LParen,   Token::None },
   { "#",        Token::Sharp,    Token::None },

   { "!",        Token::Not,        Token::None },
   { "&",        Token::Amp,        Token::None },
   { "+",        Token::Plus,       Token::None },
   { "-",        Token::Minus,      Token::None },
   { "++",       Token::PlusPlus,   Token::None },
   { "--",       Token::MinusMinus, Token::None },

   { "if",       Token::If,       Token::None },
   { "else",     Token::Else,     Token::None },
   { "while",    Token::While,    Token::None },
   { "for",      Token::For,      Token::None },
   { "switch",   Token::Switch,   Token::None },
   { "break",    Token::Break,    Token::None },
   { "continue", Token::Continue, Token::None },
   { "using",    Token::Using,    Token::None },
   { "struct",   Token::Struct,   Token::None },
   { "class",    Token::Class,    Token::None },
   { "typedef",  Token::Typedef,  Token::None },
   { "goto",     Token::Goto,     Token::None },

   { "void",     Token::Void,     Token::BasicType },
   { "int",      Token::Int,      Token::BasicType },
   { "bool",     Token::Bool,     Token::BasicType },
   { "char",     Token::Char,     Token::BasicType },
   { "float",    Token::Float,    Token::BasicType },
   { "double",   Token::Double,   Token::BasicType },
   { "string",   Token::String,   Token::BasicType },

   { "true",     Token::True,     Token::Literal },
   { "false",    Token::False,    Token::Literal },

   { "END",      Token::Unknown,  Token::None }
};

Token::Table Token::_table;

Token::Table::Table() {
   int i = 0;
   while (toktab[i].s != "END") {
      _table._map[toktab[i].s] = Token(toktab[i].t, toktab[i].k);
      i++;
   }
}

Token Token::token2type(std::string tok) {
   auto it = _table._map.find(tok);
   return (it != _table._map.end() ? it->second : Token::Unknown);
}
