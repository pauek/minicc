double    fn   (char c1 ,char   c2  )    {
   a * b + 2 * (3 = c) * d;
}
[[out]]------------------------------------
Program{
   FuncDecl("fn", Type(double), Params = {"c1": Type(char), "c2": Type(char)}, {
      Block({
         Stmt(expr, +(*(id:'a', id:'b'), *(lit:'2', *( (=(lit:'3', id:'c')) , id:'d'))))
      })
   })
}
[[err]]------------------------------------
