#ifndef TOKEN_HH
#define TOKEN_HH

#include <string>
#include <map>
#include "pos.hh"

class Token {
public:
   enum Type {
      Empty,
      Int, Void, Char, Double, Float, Bool, String,
      True, False,
      Sharp, Not, Amp, Pipe, Star, Slash, Percent, Div,
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
      Unknown
   };

   enum Group {
      None = 0, Literal = 1, TypeSpec = 2, Ident = 4, 
      Operator = 8, Control = 16, BasicType = 32, TypeQual = 64
   };
   
   static Token token2type(std::string tok);

   Type  type;
   Pos   pos;
   uint  len;
   int   group;

   bool IsIdent()     const { return group & Ident; }
   bool IsTypeSpec()  const { return group & TypeSpec; }
   bool IsBasicType() const { return group & BasicType; }
   bool IsTypeQual()  const { return group & TypeQual; }
   bool IsOperator()  const; // { return group & Operator; }

   Token(Type t = Token::Unknown, int _g = Group::None) 
      : type(t), group(_g) {}

private:
   struct Table { 
      std::map<std::string, Token> _map;
      Table(); 
   };
   static Table _table;
};


#endif
