#include <iostream>

using namespace std;

int main       /* and here */(int a  , int    b   )  {    }

[[out]]------------------------------------
Program{
   Include(<iostream>)
   Using(std)
   FuncDecl("main", Type(int), Params = {"a": Type(int), "b": Type(int)}, {
      Stmt(block, {})
   })
}
[[err]]------------------------------------
