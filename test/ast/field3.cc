int f() {
   (a+b)->c = 7;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(int), Params = {}, {
      Block({
         ExprStmt(=(FieldExpr<pointer>((+(id:'a', id:'b')), id:'c'), lit:'7'))
      })
   })
}
[[err]]--------------------------------------------------
