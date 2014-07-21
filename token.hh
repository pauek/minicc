#ifndef TOKEN_HH
#define TOKEN_HH

#include <string>
#include <map>

class Token {
public:
   enum Type {
      Unknown,
      LCurly,
      If, Else, While, For, Switch, 
      Break, Continue, Goto,
      Typedef, Class, Struct, 
      Using, 
   };
   
   static Type token2type(std::string tok);

private:
   struct Table { 
      std::map<std::string, Type> _map;
      Table(); 
   };
   static Table _table;
};


#endif
