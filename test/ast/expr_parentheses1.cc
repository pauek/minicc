double    fn   (char c1 ,char   c2  )    {
   a=(a+ 1) ;
}
[[out]]------------------------------------
Program{
   FuncDecl("fn", Type(double), Params = {"c1": Type(char), "c2": Type(char)}, {
      Block({
         ExprStmt(=(id:'a', (+(id:'a', Int<1>))))
      })
   })
}
[[err]]------------------------------------
