int f() {
   int *a;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl(id:'f', Type(id:'int'), Params = {}, {
      Block({
         DeclStmt(Type(id:'int'), Vars = {*"a"})
      })
   })
}
