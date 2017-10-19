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
      Pipe, 
      Star, 
      Slash, 
      Div,
      
      Plus, Minus, PlusPlus, MinusMinus,
      LCurly, RCurly, LParen, RParen, LBrack, RBrack, Dot, Arrow,
      If, Else, While, For, Switch, 
      Break, Continue, Goto, Return,
      Typedef, Class, Struct, Enum,
      Using, 
      StarAssign, SlashAssign, DivAssign, LShiftAssign, RShiftAssign,
      Or, BarBar, And, AmpAmp, Bar, Circum,
      Comma, Assign, PlusAssign, MinusAssign,
      AndAssign, OrAssign, XorAssign, 
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
