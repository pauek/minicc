double    fn   (char c1 ,char   c2  )    {
   a * b + 2 * (3 + c) * d;
}
[[out]]------------------------------------
Program{
   FuncDecl(id:'fn', Type(id:'double'), Params = {"c1": Type(id:'char'), "c2": Type(id:'char')}, {
      Block({
         ExprStmt(+(*(id:'a', id:'b'), *(*(Int<2>, (+(Int<3>, id:'c'))), id:'d')))
      })
   })
}
[[err]]------------------------------------
