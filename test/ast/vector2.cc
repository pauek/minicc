int f ( ) {
   vector<int>::iterator it = v.begin();
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(id:'int'), Params = {}, {
      Block({
         DeclStmt(Type(id:[id:'vector'<Type(id:'int')>]'iterator'), Vars = {"it" = CallExpr(FieldExpr(id:'v', id:'begin'), Args = {})})
      })
   })
}
[[err]]--------------------------------------------------
