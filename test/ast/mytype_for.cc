int f() {
   for (MyType x; x != false; x++) {
      x[i].set(3);
   }
}
[[out]]--------------------------------------------------
Program{
   FuncDecl(id:'f', Type(id:'int'), Params = {}, {
      Block({
         ForStmt(DeclStmt(Type(id:'MyType'), Vars = {"x"}), !=(id:'x', Bool<false>), IncrExpr<++, post>(id:'x'), {
            Block({
               ExprStmt(CallExpr(FieldExpr(IndexExpr(id:'x', id:'i'), 'set'), Args = {Int<3>}))
            })
         })
      })
   })
}
