double    fn   (char c1 ,char   c2  )    {
   a < 1 || cc != 3 || ddd >= 5 || e + d == k;
}
[[out]]------------------------------------
Program{
   FuncDecl("fn", Type(double), Params = {"c1": Type(char), "c2": Type(char)}, {
      Block({
         ExprStmt(||(||(||(<(id:'a', lit:'1'), !=(id:'cc', lit:'3')), >=(id:'ddd', lit:'5')), ==(+(id:'e', id:'d'), id:'k')))
      })
   })
}
[[err]]------------------------------------
