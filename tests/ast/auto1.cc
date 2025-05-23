int f ( ) {
   mutable int a = 1;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl(id:'f', Type(id:'int'), Params = {}, {
      Block({
         DeclStmt(Type(id:'int', {mutable}), Vars = {"a" = Int<1>})
      })
   })
}
