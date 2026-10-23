void f() {
   int a, b = a + c;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl(id:'f', Type(id:'void'), Params = {}, {
      Block({
         DeclStmt(Type(id:'int'), Vars = {"a", "b" = +(id:'a', id:'c')})
      })
   })
}
