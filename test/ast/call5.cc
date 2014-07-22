void f() {
   (a + b)(1);
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(void), Params = {}, {
      Block({
         ExprStmt(CallExpr((+(id:'a', id:'b')), Args = {lit:'1'}))
      })
   })
}
[[err]]--------------------------------------------------
