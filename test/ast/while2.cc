void xxx(string s) {
   while (a = true) a = a + 2;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("xxx", Type(void), Params = {"s": Type(string)}, {
      Block({
         IterStmt<while>(=(id:'a', lit:'true'), {
            ExprStmt(=(id:'a', +(id:'a', lit:'2')))
         })
      })
   })
}
[[err]]--------------------------------------------------
