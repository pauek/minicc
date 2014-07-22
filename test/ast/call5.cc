void f() {
   (a + b)(1);
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(void), Params = {}, {
      Block({
         Stmt(expr, CallExpr((+(id:'a', id:'b')), Args = {lit:'1'}))
      })
   })
}
[[err]]--------------------------------------------------
