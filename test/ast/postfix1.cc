int f() {
   (a+b)->c[7](1, 2, 3) = 7;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(int), Params = {}, {
      Block({
         ExprStmt(=(CallExpr(IndexExpr(FieldExpr<pointer>((+(id:'a', id:'b')), id:'c'), lit:'7'), Args = {lit:'1', lit:'2', lit:'3'}), lit:'7'))
      })
   })
}
[[err]]--------------------------------------------------
