double    fn   (char c1 ,char   c2  )    {
   a || false = b * c + d && x ^ y;
}
[[out]]------------------------------------
Program{
   FuncDecl("fn", Type(double), Params = {"c1": Type(char), "c2": Type(char)}, {
      Block({
         ExprStmt(=(||(id:'a', lit:'false'), &&(+(*(id:'b', id:'c'), id:'d'), ^(id:'x', id:'y'))))
      })
   })
}
[[err]]------------------------------------
