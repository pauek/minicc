double    fn   (char c1 ,char   c2  )    {
   a < 1 || cc != 3 || ddd >= 5 || e + d == k;
}
[[out]]------------------------------------
Program{
   FuncDecl("fn", Type(id:'double'), Params = {"c1": Type(id:'char'), "c2": Type(id:'char')}, {
      Block({
         ExprStmt(||(||(||(<(id:'a', Int<1>), !=(id:'cc', Int<3>)), >=(id:'ddd', Int<5>)), ==(+(id:'e', id:'d'), id:'k')))
      })
   })
}
[[err]]------------------------------------
