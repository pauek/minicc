#include <iostream>
using namespace std;

int main() {
   int a[2] = {1, 1}, b[3][4], c[5][2];
}
[[out]]--------------------------------------------------
Program{
   Include(<iostream>)
   Using(std)
   FuncDecl(id:'main', Type(id:'int'), Params = {}, {
      Block({
         DeclStmt(Type(id:'int'), Vars = {"a"(Size = Int<2>) = {Int<1>, Int<1>}, "b"(Sizes = {Int<3>, Int<4>}), "c"(Sizes = {Int<5>, Int<2>})})
      })
   })
}
