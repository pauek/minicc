void f() {
   a ? true : false, b ? 3 : 5;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl(id:'f', Type(id:'void'), Params = {}, {
      Block({
         ExprStmt(,(CondExpr(id:'a', Bool<true>, Bool<false>), CondExpr(id:'b', Int<3>, Int<5>)))
      })
   })
}
[[err]]--------------------------------------------------
