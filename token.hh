#ifndef TOKEN_HH
#define TOKEN_HH

#include <string>
#include <map>

class Token {
public:
   enum Type {
      Unknown, Empty,
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
      IntLiteral      
   };

   enum Kind {
      Literal, BasicType, None
   };
   
   static Token token2type(std::string tok);

   int ini, fin;
   Type t;
   Kind k;

   Token(Type _t = Unknown, Kind _k = None) : t(_t), k(_k), ini(-1), fin(-1) {}

private:
   struct Table { 
      std::map<std::string, Token> _map;
      Table(); 
   };
   static Table _table;
};


#endif
