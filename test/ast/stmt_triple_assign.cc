double    fn   (char c1 ,float x  )    {
   a= b  = c = 1  ;
}
[[out]]------------------------------------
Program{
   FuncDecl(id:'fn', Type(id:'double'), Params = {"c1": Type(id:'char'), "x": Type(id:'float')}, {
      Block({
         ExprStmt(=(id:'a', =(id:'b', =(id:'c', Int<1>))))
      })
   })
}
[[err]]------------------------------------
