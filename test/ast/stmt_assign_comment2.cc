double    fn   (char c1 ,char   c2  )    {
   a=/* 1234 */1  ;
}
[[out]]------------------------------------
Program{
   FuncDecl("fn", Type(double), Params = {"c1": Type(char), "c2": Type(char)}, {
      Stmt(block, {
         Stmt(expr, =(id:'a', lit:'1'))
      })
   })
}
[[err]]------------------------------------
