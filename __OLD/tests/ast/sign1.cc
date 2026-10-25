double    fn   (char c1 ,char   c2  )    {
   -a + b;
}
[[out]]------------------------------------
Program{
   FuncDecl(id:'fn', Type(id:'double'), Params = {"c1": Type(id:'char'), "c2": Type(id:'char')}, {
      Block({
         ExprStmt(+(SignExpr<->(id:'a'), id:'b'))
      })
   })
}
