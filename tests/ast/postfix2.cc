int f() {
   a.t[1+5].x->z = 1;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl(id:'f', Type(id:'int'), Params = {}, {
      Block({
         ExprStmt(=(FieldExpr<pointer>(FieldExpr(IndexExpr(FieldExpr(id:'a', 't'), +(Int<1>, Int<5>)), 'x'), 'z'), Int<1>))
      })
   })
}
