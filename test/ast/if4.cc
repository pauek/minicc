int f() {
   if (true) a + 1; 
   else bla + 2;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl(id:'f', Type(id:'int'), Params = {}, {
      Block({
         IfStmt(Bool<true>, ExprStmt(+(id:'a', Int<1>)), ExprStmt(+(id:'bla', Int<2>)))
      })
   })
}
[[err]]--------------------------------------------------
