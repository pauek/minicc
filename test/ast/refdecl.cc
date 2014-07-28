char f() {
   int& a;
}
[[out]]------------------------------------
Program{
   FuncDecl("f", Type(id:'char'), Params = {}, {
      Block({
         DeclStmt(Type<&>(id:'int'), Vars = {"a"})
      })
   })
}
[[err]]------------------------------------
