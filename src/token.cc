
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
   case Token::Eq:
   case Token::StarEq:
   case Token::MinusEq:
   case Token::PlusEq:
   case Token::SlashEq:
   case Token::DivEq:
   case Token::BarEq:
   case Token::AmpEq:
   case Token::XorEq:
   case Token::LShiftEq:
   case Token::RShiftEq:
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
   case Token::Bar:
   case Token::Xor:
   case Token::Plus:
   case Token::Minus:
   case Token::Star:
   case Token::Slash:
   case Token::Div:
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
