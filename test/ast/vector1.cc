int f ( ) {
   const vector<int> v;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(id:'int'), Params = {}, {
      Block({
         DeclStmt(Type(Template(id:'vector', Args = {Type(id:'int')}), {const}), Vars = {"v"})
      })
   })
}
[[err]]--------------------------------------------------
