#include <iostream>
using namespace std;

double func(int a, int b, int c) {
   a = 1;
   b = 2;
   c = 3;
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
         Stmt(expr, =(id:'a', lit:'1'))
         Stmt(expr, =(id:'b', lit:'2'))
         Stmt(expr, =(id:'c', lit:'3'))
      })
   })
   FuncDecl("reverse", Type(string), Params = {"s": Type(string)}, {
      Block({
         Stmt(expr, =(id:'r', id:'s'))
      })
   })
   FuncDecl("main", Type(int), Params = {}, {
      Block({})
   })
}
[[err]]------------------------------------
