int f ( ) {
    const map<int, string> M;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl(id:'f', Type(id:'int'), Params = {}, {
      Block({
         DeclStmt(Type(id:'map'<Type(id:'int'), Type(id:'string')>, {const}), Vars = {"M"})
      })
   })
}
