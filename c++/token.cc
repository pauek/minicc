
#include <iostream>
#include "token.hh"
using namespace std;

// OJO: El orden de la tabla es importante!
// Hay que dejarla antes que el _table...
//
struct { 
   string      s; 
   Token::Kind t; 
   int         k;
} toktab[] = {
   { "",           Token::Empty, Token::None },

   { ";",      Token::SemiColon, Token::None },
   { ":",          Token::Colon, Token::None },
   { "::",    Token::ColonColon, Token::None },
   { ",",          Token::Comma, Token::Operator },
   { "?",          Token::QMark, Token::Operator },

   { "||",        Token::BarBar, Token::Operator },
   { "&&",        Token::AmpAmp, Token::Operator },
   { "=",         Token::Assign, Token::Operator },
   { "*=",    Token::StarAssign, Token::Operator },
   { "-=",   Token::MinusAssign, Token::Operator },
   { "+=",    Token::PlusAssign, Token::Operator },
   { "/=",   Token::SlashAssign, Token::Operator },
   { "%=",     Token::DivAssign, Token::Operator },
   { "|=",      Token::OrAssign, Token::Operator },
   { "&=",     Token::AndAssign, Token::Operator },
   { "^=",     Token::XorAssign, Token::Operator },
   { "<<=", Token::LShiftAssign, Token::Operator },
   { ">>=", Token::RShiftAssign, Token::Operator },
   { "or",            Token::Or, Token::Operator },
   { "and",          Token::And, Token::Operator },

   { "==",          Token::EqEq, Token::Operator },
   { "!=",         Token::NotEq, Token::Operator },
   { "<",             Token::LT, Token::Operator },
   { ">",             Token::GT, Token::Operator },
   { ">=",            Token::GE, Token::Operator },
   { "<=",            Token::LE, Token::Operator },
   { "<<",        Token::LShift, Token::Operator },
   { ">>",        Token::RShift, Token::Operator },

   { ".",        Token::Dot,      Token::Operator },
   { "->",       Token::Arrow,    Token::Operator },

   { "!",        Token::Not,        Token::Operator },
   { "&",        Token::Amp,        Token::Operator },
   { "|",        Token::Pipe,       Token::Operator },
   { "^",        Token::Circum,     Token::Operator },
   { "+",        Token::Plus,       Token::Operator },
   { "-",        Token::Minus,      Token::Operator },
   { "*",        Token::Star,       Token::Operator },
   { "/",        Token::Slash,      Token::Operator },
   { "%",        Token::Percent,    Token::Operator },
   { "++",       Token::PlusPlus,   Token::Operator },
   { "--",       Token::MinusMinus, Token::Operator },

   { "#",        Token::Sharp,    Token::None },
   { "{",        Token::LCurly,   Token::None },
   { "}",        Token::RCurly,   Token::None },
   { "[",        Token::LBrack,   Token::None },
   { "(",        Token::LParen,   Token::None },

   { "if",       Token::If,       Token::Control },
   { "else",     Token::Else,     Token::Control },
   { "while",    Token::While,    Token::Control },
   { "for",      Token::For,      Token::Control },
   { "switch",   Token::Switch,   Token::Control },
   { "break",    Token::Break,    Token::Control },
   { "continue", Token::Continue, Token::Control },
   { "goto",     Token::Goto,     Token::Control },
   { "return",   Token::Return,   Token::Control },

   { "using",    Token::Using,    Token::None },
   { "struct",   Token::Struct,   Token::None },
   { "class",    Token::Class,    Token::None },
   { "typedef",  Token::Typedef,  Token::None },
   { "enum",     Token::Enum,     Token::None },

   // simple_type_specifier
   { "void",     Token::Void,     Token::TypeSpec | Token::BasicType },
   { "int",      Token::Int,      Token::TypeSpec | Token::BasicType },
   { "short",    Token::Short,    Token::TypeSpec | Token::BasicType },
   { "long",     Token::Long,     Token::TypeSpec | Token::BasicType },
   { "bool",     Token::Bool,     Token::TypeSpec | Token::BasicType },
   { "char",     Token::Char,     Token::TypeSpec | Token::BasicType },
   { "float",    Token::Float,    Token::TypeSpec | Token::BasicType },
   { "double",   Token::Double,   Token::TypeSpec | Token::BasicType },
   { "string",   Token::String,   Token::TypeSpec | Token::BasicType | Token::Ident },

   { "signed",   Token::Signed,   Token::TypeSpec },
   { "unsigned", Token::Unsigned, Token::TypeSpec },

   { "const",    Token::Const,    Token::TypeSpec | Token::TypeQual },
   { "long",     Token::Long,     Token::TypeSpec | Token::TypeQual },
   { "volatile", Token::Volatile, Token::TypeSpec | Token::TypeQual },

   // storage_class_specifier
   { "auto",     Token::Auto,     Token::TypeSpec | Token::TypeQual },
   { "register", Token::Register, Token::TypeSpec | Token::TypeQual },
   { "static",   Token::Static,   Token::TypeSpec | Token::TypeQual },
   { "extern",   Token::Extern,   Token::TypeSpec | Token::TypeQual },
   { "mutable",  Token::Mutable,  Token::TypeSpec | Token::TypeQual },

   // function_specifier
   { "inline",   Token::Inline,   Token::TypeSpec },
   { "virtual",  Token::Virtual,  Token::TypeSpec },
   { "explicit", Token::Explicit, Token::TypeSpec },

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
   return (it != _table._map.end() ? it->second : Token(Token::Unknown, Token::Ident));
}
