void f() {
   (a + b)++;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(id:'void'), Params = {}, {
      Block({
         ExprStmt(IncrExpr<++, post>((+(id:'a', id:'b'))))
      })
   })
}
[[err]]--------------------------------------------------
