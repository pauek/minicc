void xxx(string s) {
   while (a = true) a = a + 2;
   while (a = true) a = a + 2;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl(id:'xxx', Type(id:'void'), Params = {"s": Type(id:'string')}, {
      Block({
         WhileStmt(=(id:'a', Bool<true>), {
            ExprStmt(=(id:'a', +(id:'a', Int<2>)))
         })
         WhileStmt(=(id:'a', Bool<true>), {
            ExprStmt(=(id:'a', +(id:'a', Int<2>)))
         })
      })
   })
}
