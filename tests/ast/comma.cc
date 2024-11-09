int f() {
   a = 1, b = 2;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl(id:'f', Type(id:'int'), Params = {}, {
      Block({
         ExprStmt(,(=(id:'a', Int<1>), =(id:'b', Int<2>)))
      })
   })
}
