void f() {
   int a = 2, b;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl(id:'f', Type(id:'void'), Params = {}, {
      Block({
         DeclStmt(Type(id:'int'), Vars = {"a" = Int<2>, "b"})
      })
   })
}
