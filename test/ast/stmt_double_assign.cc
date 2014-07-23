double    fn   (char c1 ,char   c2  )    {
   a=b=1  ;
}
[[out]]------------------------------------
Program{
   FuncDecl("fn", Type(id:'double'), Params = {"c1": Type(id:'char'), "c2": Type(id:'char')}, {
      Block({
         ExprStmt(=(id:'a', =(id:'b', Int<1>)))
      })
   })
}
[[err]]------------------------------------
