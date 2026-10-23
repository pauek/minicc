int f() {
   MyType x, y;
   x.set(3);
}
[[out]]--------------------------------------------------
Program{
   FuncDecl(id:'f', Type(id:'int'), Params = {}, {
      Block({
         DeclStmt(Type(id:'MyType'), Vars = {"x", "y"})
         ExprStmt(CallExpr(FieldExpr(id:'x', 'set'), Args = {Int<3>}))
      })
   })
}
