void f() {
   int a, b = a + c;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(void), Params = {}, {
      Block({
         DeclStmt(Type(int), Vars = {"a", "b" = +(id:'a', id:'c')})
      })
   })
}
[[err]]--------------------------------------------------
