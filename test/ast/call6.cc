void f() {
   a + b(1);
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(void), Params = {}, {
      Block({
         ExprStmt(+(id:'a', CallExpr(id:'b', Args = {Int<1>})))
      })
   })
}
[[err]]--------------------------------------------------
