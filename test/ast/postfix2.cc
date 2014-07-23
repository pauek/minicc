int f() {
   a.t[1+5].x->z = 1;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(id:'int'), Params = {}, {
      Block({
         ExprStmt(=(FieldExpr<pointer>(FieldExpr(IndexExpr(FieldExpr(id:'a', id:'t'), +(Int<1>, Int<5>)), id:'x'), id:'z'), Int<1>))
      })
   })
}
[[err]]--------------------------------------------------
