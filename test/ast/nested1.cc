int f ( ) {
   a::b::c x = 1;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl(id:'f', Type(id:'int'), Params = {}, {
      Block({
         DeclStmt(Type(id:[id:'a', id:'b']'c'), Vars = {"x" = Int<1>})
      })
   })
}
[[err]]--------------------------------------------------
