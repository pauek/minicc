const int a = 3;

void f() { g(); }
[[out]]--------------------------------------------------
Program{
   DeclStmt(Type(id:'int', {const}), Vars = {"a" = Int<3>})
   FuncDecl("f", Type(id:'void'), Params = {}, {
      Block({
         ExprStmt(CallExpr(id:'g', Args = {}))
      })
   })
}
[[err]]--------------------------------------------------
