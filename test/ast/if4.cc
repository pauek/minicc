int f() {
   if (true) a + 1; 
   else bla + 2;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(int), Params = {}, {
      Block({
         IfStmt(lit:'true', Stmt(expr, +(id:'a', lit:'1')), Stmt(expr, +(id:'bla', lit:'2')))
      })
   })
}
[[err]]--------------------------------------------------
