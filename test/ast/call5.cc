void f() {
   (a + b)(1);
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(void), Params = {}, {
      Block({
         ExprStmt(CallExpr((+(id:'a', id:'b')), Args = {Int<1>}))
      })
   })
}
[[err]]--------------------------------------------------
