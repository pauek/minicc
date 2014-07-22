int f() {
   a.t[1+5].x->z = 1;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(int), Params = {}, {
      Block({
         ExprStmt(=(FieldExpr<pointer>(FieldExpr(IndexExpr(FieldExpr(id:'a', id:'t'), +(lit:'1', lit:'5')), id:'x'), id:'z'), lit:'1'))
      })
   })
}
[[err]]--------------------------------------------------
