double    fn   (char c1 ,char   c2  )    {
   &a->b;
}
[[out]]------------------------------------
Program{
   FuncDecl("fn", Type(double), Params = {"c1": Type(char), "c2": Type(char)}, {
      Block({
         ExprStmt(AddrExpr(FieldExpr<pointer>(id:'a', id:'b')))
      })
   })
}
[[err]]------------------------------------
