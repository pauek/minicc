double    fn   (char c1 ,char   c2  )    {
   cout << a + 1 << x < y << endl;
}
[[out]]------------------------------------
Program{
   FuncDecl("fn", Type(double), Params = {"c1": Type(char), "c2": Type(char)}, {
      Block({
         ExprStmt(<(<<(<<(id:'cout', +(id:'a', Int<1>)), id:'x'), <<(id:'y', id:'endl')))
      })
   })
}
[[err]]------------------------------------
