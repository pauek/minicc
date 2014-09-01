void f() {
   g(a + 1, b + 2);
}
[[out]]--------------------------------------------------
Program{
   FuncDecl(id:'f', Type(id:'void'), Params = {}, {
      Block({
         ExprStmt(CallExpr(id:'g', Args = {+(id:'a', Int<1>), +(id:'b', Int<2>)}))
      })
   })
}
[[err]]--------------------------------------------------
