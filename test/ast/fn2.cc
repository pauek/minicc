#include <iostream>
using namespace std;

double func(int a, int b, int c) {
   a = 1;
   b += 2;
   c /= 3;
}

string reverse(string s) {
   r = s;
}

int main() {

}

[[out]]------------------------------------
Program{
   Include(<iostream>)
   Using(std)
   FuncDecl("func", Type(double), Params = {"a": Type(int), "b": Type(int), "c": Type(int)}, {
      Block({
         ExprStmt(=(id:'a', Int<1>))
         ExprStmt(+=(id:'b', Int<2>))
         ExprStmt(/=(id:'c', Int<3>))
      })
   })
   FuncDecl("reverse", Type(string), Params = {"s": Type(string)}, {
      Block({
         ExprStmt(=(id:'r', id:'s'))
      })
   })
   FuncDecl("main", Type(int), Params = {}, {
      Block({})
   })
}
[[err]]------------------------------------
