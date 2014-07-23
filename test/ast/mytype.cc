int f() {
   MyType x, y;
   x.set(3);
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(int), Params = {}, {
      Block({
         DeclStmt(Type(MyType), Vars = {"x", "y"})
         ExprStmt(CallExpr(FieldExpr(id:'x', id:'set'), Args = {Int<3>}))
      })
   })
}
[[err]]--------------------------------------------------
