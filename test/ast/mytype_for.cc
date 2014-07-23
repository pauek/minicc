int f() {
   for (MyType x; x != false; x++) {
      x[i].set(3);
   }
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(int), Params = {}, {
      Block({
         IterStmt<for>(DeclStmt(Type(MyType), Vars = {"x"}), !=(id:'x', Bool<false>), IncrExpr<++, post>(id:'x'), {
            Block({
               ExprStmt(CallExpr(FieldExpr(IndexExpr(id:'x', id:'i'), id:'set'), Args = {Int<3>}))
            })
         })
      })
   })
}
[[err]]--------------------------------------------------
