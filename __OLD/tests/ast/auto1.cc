int f ( ) {
   auto mutable int a = 1;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl(id:'f', Type(id:'int'), Params = {}, {
      Block({
         DeclStmt(Type(id:'int', {mutable, auto}), Vars = {"a" = Int<1>})
      })
   })
}
