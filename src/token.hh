#ifndef TOKEN_HH
#define TOKEN_HH

#include "pos.hh"

struct Token 
{
   enum Type {
      Empty,
      Int, 
      Void, 
      Char, 
      Double, 
      Float, 
      Bool, 
      String,
      
      True, 
      False,

      Sharp, 
      Not, 
      Amp, 
      Bar,
      Star, 
      Slash, 
      Div,

      Plus, 
      Minus, 
      PlusPlus, 
      MinusMinus,

      LBrace, 
      RBrace, 
      LParen, 
      RParen, 
      LBracket, 
      RBracket, 
      Dot, 
      Arrow,

      If, Else, While, For, Switch, 
      Break, Continue, Goto, Return,
      Typedef, Class, Struct, Enum,
      Using,
      
      StarEq, SlashEq, DivEq, LShiftEq, RShiftEq,
      Or, BarBar, And, AmpAmp, Circum,
      Comma, Eq, PlusEq, MinusEq,
      AmpEq, BarEq, XorEq, 
      EqEq, NotEq, LT, GT, LE, GE, LShift, RShift,
      IntLiteral, CharLiteral, StringLiteral, 
      FloatLiteral, DoubleLiteral,
      Signed, Unsigned, Volatile, Const, Short, Long,
      Inline, Virtual, Explicit,
      Auto, Register, Static, Extern, Mutable,
      Colon, ColonColon, SemiColon, QMark,
      Ident,
      Unknown
   };
   
   Type  type;
   Pos   pos;
   uint  len;
   int   group;

   Token(Type t = Token::Unknown) : type(t) {}

   bool IsIdent()     const { return type == Ident or type == String; }
   bool IsTypeSpec()  const;
   bool IsBasicType() const;
   bool IsTypeQual()  const;
   bool IsOperator()  const;
};

#endif
