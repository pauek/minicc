#include <iostream>
using namespace blah;

double    fn   (char c1 ,char   c2  )    {
   a= /* b + */ a+ 1 ;
   b = b + 2;
   c = c + 3;
}
[[out]]------------------------------------
Program{
   Include(<iostream>)
   Using(blah)
   FuncDecl("fn", Type(double), Params = {"c1": Type(char), "c2": Type(char)}, {
      Stmt(block, {
         Stmt(expr, =(id:'a', +(id:'a', lit:'1')))
         Stmt(expr, =(id:'b', +(id:'b', lit:'2')))
         Stmt(expr, =(id:'c', +(id:'c', lit:'3')))
      })
   })
}
[[err]]------------------------------------
