void f() {
   int a = 2, b;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(void), Params = {}, {
      Block({
         DeclStmt(Type(int), Vars = {"a" = Int<2>, "b"})
      })
   })
}
[[err]]--------------------------------------------------
