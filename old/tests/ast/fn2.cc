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
   FuncDecl(id:'func', Type(id:'double'), Params = {"a": Type(id:'int'), "b": Type(id:'int'), "c": Type(id:'int')}, {
      Block({
         ExprStmt(=(id:'a', Int<1>))
         ExprStmt(+=(id:'b', Int<2>))
         ExprStmt(/=(id:'c', Int<3>))
      })
   })
   FuncDecl(id:'reverse', Type(id:'string'), Params = {"s": Type(id:'string')}, {
      Block({
         ExprStmt(=(id:'r', id:'s'))
      })
   })
   FuncDecl(id:'main', Type(id:'int'), Params = {}, {
      Block({})
   })
}
