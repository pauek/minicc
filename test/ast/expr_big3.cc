double    fn   (char c1 ,char   c2  )    {
   cout << a + 1 << x < y << endl;
}
[[out]]------------------------------------
Program{
   FuncDecl("fn", Type(double), Params = {"c1": Type(char), "c2": Type(char)}, {
      Block({
         Stmt(expr, <(<<(<<(id:'cout', +(id:'a', lit:'1')), id:'x'), <<(id:'y', id:'endl')))
      })
   })
}
[[err]]------------------------------------
