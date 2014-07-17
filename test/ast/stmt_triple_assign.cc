double    fn   (char c1 ,float x  )    {
   a= b  = c = 1  ;
}
[[out]]------------------------------------
Program{
   FuncDecl("fn", Type(double), Params = {"c1": Type(char), "x": Type(float)}, {
      Stmt(block, {
         Stmt(expr, =(id:'a', =(id:'b', =(id:'c', lit:'1'))))
      })
   })
}
[[err]]------------------------------------
