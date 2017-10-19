
#include <iostream>
#include "token.hh"
using namespace std;

bool Token::IsTypeSpec() const {
   switch (type) {
   case Token::Void:
   case Token::Int:
   case Token::Short:
   case Token::Long:
   case Token::Bool:
   case Token::Char:
   case Token::Float:
   case Token::Double:
   case Token::String:
   case Token::Signed:
   case Token::Unsigned:
   case Token::Const:
   case Token::Volatile:
   case Token::Auto:
   case Token::Register:
   case Token::Static:
   case Token::Extern:
   case Token::Mutable:
   case Token::Inline:
   case Token::Virtual:
   case Token::Explicit:
      return true;
   }
   return false;
}

bool Token::IsOperator() const {
   switch (type) {
   case Token::Comma:
   case Token::QMark:
   case Token::BarBar:
   case Token::AmpAmp:
   case Token::Assign:
   case Token::StarAssign:
   case Token::MinusAssign:
   case Token::PlusAssign:
   case Token::SlashAssign:
   case Token::DivAssign:
   case Token::OrAssign:
   case Token::AndAssign:
   case Token::XorAssign:
   case Token::LShiftAssign:
   case Token::RShiftAssign:
   case Token::Or:
   case Token::And:
   case Token::EqEq:
   case Token::NotEq:
   case Token::LT:
   case Token::GT:
   case Token::GE:
   case Token::LE:
   case Token::LShift:
   case Token::RShift:
   case Token::Dot:
   case Token::Arrow:
   case Token::Not:
   case Token::Amp:
   case Token::Pipe:
   case Token::Circum:
   case Token::Plus:
   case Token::Minus:
   case Token::Star:
   case Token::Slash:
   case Token::Percent:
   case Token::PlusPlus:
   case Token::MinusMinus:
      return true;
   }
   return false;
}

bool Token::IsBasicType() const {
   switch (type) {
   case Token::Void:
   case Token::Int:
   case Token::Short:
   case Token::Long:
   case Token::Bool:
   case Token::Char:
   case Token::Float:
   case Token::Double:
   case Token::String:
      return true;
   }
   return false;
}

bool Token::IsTypeQual() const {
   switch (type) {
   case Token::Const:
   case Token::Long:
   case Token::Volatile:
   case Token::Auto:
   case Token::Register:
   case Token::Static:
   case Token::Extern:
   case Token::Mutable:
      return true;
   }
   return false;
}

struct TokenInfo { 
   string      s; 
   Token::Type t; 
   int         k;
};

// OJO: El orden de la tabla es importante!
// Hay que dejarla antes que el _table...
//
TokenInfo toktab[] = {
   { "switch",   Token::Switch,   Token::Control },
   { "continue", Token::Continue, Token::Control },
   { "return",   Token::Return,   Token::Control },

   { "struct",   Token::Struct,   Token::None },
   { "typedef",  Token::Typedef,  Token::None },

   // simple_type_specifier
   { "double",   Token::Double,   Token::TypeSpec | Token::BasicType },
   { "string",   Token::String,   Token::TypeSpec | Token::BasicType | Token::Ident },

   { "signed",   Token::Signed,   Token::TypeSpec },
   { "unsigned", Token::Unsigned, Token::TypeSpec },

   { "volatile", Token::Volatile, Token::TypeSpec | Token::TypeQual },

   // storage_class_specifier
   { "register", Token::Register, Token::TypeSpec | Token::TypeQual },
   { "static",   Token::Static,   Token::TypeSpec | Token::TypeQual },
   { "extern",   Token::Extern,   Token::TypeSpec | Token::TypeQual },
   { "mutable",  Token::Mutable,  Token::TypeSpec | Token::TypeQual },

   // function_specifier
   { "inline",   Token::Inline,   Token::TypeSpec },
   { "virtual",  Token::Virtual,  Token::TypeSpec },
   { "explicit", Token::Explicit, Token::TypeSpec },

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
   return (it != _table._map.end() ? it->second : Token(Token::Ident, Token::Ident));
}
