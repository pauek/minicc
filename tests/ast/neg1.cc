double    fn   (char c1 ,char   c2  )    {
   !a;
}
[[out]]------------------------------------
Program{
   FuncDecl(id:'fn', Type(id:'double'), Params = {"c1": Type(id:'char'), "c2": Type(id:'char')}, {
      Block({
         ExprStmt(NegExpr(id:'a'))
      })
   })
}
