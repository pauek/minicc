#include <iostream>
using namespace std;

int main() {
   string s;
   cin >> s;
   if (s == "a") {
      cout << 'A' << endl;
   } else if (s == "b") {
      cout << 'B' << endl;
   } else if (s == "c") {
      cout << 'C' << endl;
   }
}
[[out]]--------------------------------------------------
Program{
   Include(<iostream>)
   Using(std)
   FuncDecl("main", Type(id:'int'), Params = {}, {
      Block({
         DeclStmt(Type(id:'string'), Vars = {"s"})
         ExprStmt(>>(id:'cin', id:'s'))
         IfStmt(==(id:'s', String<a>), Block({
            ExprStmt(<<(<<(id:'cout', Char<A>), id:'endl'))
         }), IfStmt(==(id:'s', String<b>), Block({
            ExprStmt(<<(<<(id:'cout', Char<B>), id:'endl'))
         }), IfStmt(==(id:'s', String<c>), Block({
            ExprStmt(<<(<<(id:'cout', Char<C>), id:'endl'))
         }))))
      })
   })
}
[[err]]--------------------------------------------------
