double    fn   (char c1 ,char   c2  )    {
   a * b + 2 * (3 + c) * d;
}
[[out]]------------------------------------
Program{
   FuncDecl("fn", Type(double), Params = {"c1": Type(char), "c2": Type(char)}, {
      Block({
         ExprStmt(+(*(id:'a', id:'b'), *(*(Int<2>, (+(Int<3>, id:'c'))), id:'d')))
      })
   })
}
[[err]]------------------------------------
