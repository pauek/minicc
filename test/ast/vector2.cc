int f ( ) {
   vector<int>::iterator it = v.begin();
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(id:'int'), Params = {}, {
      Block({
         DeclStmt(Type([Template(id:'vector', Args = {Type(id:'int')}), id:'iterator']), Vars = {"it" = CallExpr(FieldExpr(id:'v', id:'begin'), Args = {})})
      })
   })
}
[[err]]--------------------------------------------------
