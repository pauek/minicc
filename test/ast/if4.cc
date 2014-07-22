int f() {
   if (true) a + 1; 
   else bla + 2;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(int), Params = {}, {
      Block({
         IfStmt(lit:'true', ExprStmt(+(id:'a', lit:'1')), ExprStmt(+(id:'bla', lit:'2')))
      })
   })
}
[[err]]--------------------------------------------------
