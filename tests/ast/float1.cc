void f() {
   float x;
   x = 1.0f;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl(id:'f', Type(id:'void'), Params = {}, {
      Block({
         DeclStmt(Type(id:'float'), Vars = {"x"})
         ExprStmt(=(id:'x', Float<1>))
      })
   })
}
