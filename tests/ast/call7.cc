void f() {
   a + b(1, "blabla\n");
}
[[out]]--------------------------------------------------
Program{
   FuncDecl(id:'f', Type(id:'void'), Params = {}, {
      Block({
         ExprStmt(+(id:'a', CallExpr(id:'b', Args = {Int<1>, String<blabla\n>})))
      })
   })
}
