int f ( ) {
    const map<int, string> M;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(id:'int'), Params = {}, {
      Block({
         DeclStmt(Type(Template(id:'map', Args = {Type(id:'int'), Type(id:'string')}), {const}), Vars = {"M"})
      })
   })
}
[[err]]--------------------------------------------------
