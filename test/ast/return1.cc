void f() {
   return 1;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(id:'void'), Params = {}, {
      Block({
         ExprStmt<return>(Int<1>)
      })
   })
}
[[err]]--------------------------------------------------
