int f ( ) {
   a::b::c x = 1;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(id:'int'), Params = {}, {
      Block({
         DeclStmt(Type([id:'a', id:'b', id:'c']), Vars = {"x" = Int<1>})
      })
   })
}
[[err]]--------------------------------------------------
