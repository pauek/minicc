#include <iostream>
using namespace std;

int main() {
   int a[6] = {1, 1, 1, 1, 1, 1};
   int b = 2, c = 3;
   a[0] &= b; a[1] &= c;
   a[2] |= b; a[3] |= c;
   a[4] ^= b; a[5] ^= c;
   for (int i = 0; i < 3; i++) {
      cout << a[2*i] << ' ' << a[2*i+1] << endl;
   }
}
[[out]]--------------------------------------------------
Program{
   Include(<iostream>)
   Using(std)
   FuncDecl(id:'main', Type(id:'int'), Params = {}, {
      Block({
         DeclStmt(Type(id:'int'), Vars = {"a"(Size = Int<6>) = {Int<1>, Int<1>, Int<1>, Int<1>, Int<1>, Int<1>}})
         DeclStmt(Type(id:'int'), Vars = {"b" = Int<2>, "c" = Int<3>})
         ExprStmt(&=(IndexExpr(id:'a', Int<0>), id:'b'))
         ExprStmt(&=(IndexExpr(id:'a', Int<1>), id:'c'))
         ExprStmt(|=(IndexExpr(id:'a', Int<2>), id:'b'))
         ExprStmt(|=(IndexExpr(id:'a', Int<3>), id:'c'))
         ExprStmt(^=(IndexExpr(id:'a', Int<4>), id:'b'))
         ExprStmt(^=(IndexExpr(id:'a', Int<5>), id:'c'))
         ForStmt(DeclStmt(Type(id:'int'), Vars = {"i" = Int<0>}), <(id:'i', Int<3>), IncrExpr<++, post>(id:'i'), {
            Block({
               ExprStmt(<<(<<(<<(<<(id:'cout', IndexExpr(id:'a', *(Int<2>, id:'i'))), Char< >), IndexExpr(id:'a', +(*(Int<2>, id:'i'), Int<1>))), id:'endl'))
            })
         })
      })
   })
}
