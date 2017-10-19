#ifndef TOKEN_HH
#define TOKEN_HH

#include "pos.hh"

struct Token 
{
   enum Type {
      Empty, 
      Ident,
      True, False,
      Void, Bool, Char, Int, Double, Float, String,
      Sharp, Not, Amp, Bar, Star, Slash, Div,
      Plus, Minus, PlusPlus, MinusMinus,
      RBrace, LBrace, LParen, RParen, LBracket, RBracket, Dot, Arrow,
      If, Else, While, For, Switch, 
      Break, Continue, Goto, Return,
      Typedef, Class, Struct, Enum,
      Using,
      StarEq, SlashEq, DivEq, LShiftEq, RShiftEq,
      Or, BarBar, And, AmpAmp, Xor,
      Comma, Eq, PlusEq, MinusEq,
      Colon, ColonColon, SemiColon, QMark,
      AmpEq, BarEq, XorEq, 
      EqEq, NotEq, LT, GT, LE, GE, 
      LShift, RShift,
      IntLiteral, CharLiteral, StringLiteral, FloatLiteral, DoubleLiteral,
      Signed, Unsigned, Volatile, Const, Short, Long,
      Inline, Virtual, Explicit,
      Auto, Register, Static, Extern, Mutable,
      Unknown
   };
   
   Type  type;
   Pos   pos;
   uint  len;

   Token(Type t = Token::Unknown) : type(t) {}

   bool IsIdent()     const { return type == Ident or type == String; }
   bool IsTypeSpec()  const;
   bool IsBasicType() const;
   bool IsTypeQual()  const;
   bool IsOperator()  const;
};

#endif
