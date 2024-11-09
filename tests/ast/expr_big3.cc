double    fn   (char c1 ,char   c2  )    {
   cout << a + 1 << x < y << endl;
}
[[out]]------------------------------------
Program{
   FuncDecl(id:'fn', Type(id:'double'), Params = {"c1": Type(id:'char'), "c2": Type(id:'char')}, {
      Block({
         ExprStmt(<(<<(<<(id:'cout', +(id:'a', Int<1>)), id:'x'), <<(id:'y', id:'endl')))
      })
   })
}
