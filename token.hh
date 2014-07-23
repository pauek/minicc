#ifndef TOKEN_HH
#define TOKEN_HH

#include <string>
#include <map>

class Token {
public:
   enum Type {
      Empty,
      Int, Void, Char, Double, Float, Bool, String,
      True, False,
      Sharp, Not, Amp, Star, Slash, Div,
      Plus, Minus, PlusPlus, MinusMinus,
      LCurly, LParen, LBrack, Dot, Arrow,
      If, Else, While, For, Switch, 
      Break, Continue, Goto,
      Typedef, Class, Struct, 
      Using, 
      StarAssign, SlashAssign, DivAssign, LShiftAssign, RShiftAssign,
      Or, BarBar, And, AmpAmp, Bar, Circum,
      Comma, Assign, PlusAssign, MinusAssign,
      AndAssign, OrAssign, XorAssign, 
      EqEq, NotEq, LT, GT, LE, GE, LShift, RShift,
      IntLiteral, CharLiteral, StringLiteral, RealLiteral,
      Signed, Unsigned, Volatile, Const, Short, Long,
      Inline, Virtual, Explicit,
      Auto, Register, Static, Extern, Mutable,
      Unknown
   };

   enum Kind {
      None = 0, Literal = 1, TypeSpec = 2, Identifier = 4, 
      Operator = 8, Control = 16, BasicType = 32, TypeQual = 64
   };
   
   static Token token2type(std::string tok);

   int ini, fin;
   Type t;
   int k;
   std::string str;

   Token(Type _t = Unknown, int _k = None) : t(_t), k(_k), ini(-1), fin(-1) {}

private:
   struct Table { 
      std::map<std::string, Token> _map;
      Table(); 
   };
   static Table _table;
};


#endif
