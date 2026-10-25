void f() {
   (a + b)(1);
}
[[out]]--------------------------------------------------
Program{
   FuncDecl(id:'f', Type(id:'void'), Params = {}, {
      Block({
         ExprStmt(CallExpr((+(id:'a', id:'b')), Args = {Int<1>}))
      })
   })
}
