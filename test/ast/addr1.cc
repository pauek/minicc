double    fn   (char c1 ,char   c2  )    {
   &cout;
}
[[out]]------------------------------------
Program{
   FuncDecl("fn", Type(double), Params = {"c1": Type(char), "c2": Type(char)}, {
      Block({
         ExprStmt(AddrExpr(id:'cout'))
      })
   })
}
[[err]]------------------------------------
