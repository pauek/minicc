int f ( ) {
    const map<int, vector<string>> M;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(id:'int'), Params = {}, {
      Block({
         DeclStmt(Type(id:'map'<Type(id:'int'), Type(id:'vector'<Type(id:'string')>)>, {const}), Vars = {"M"})
      })
   })
}
[[err]]--------------------------------------------------
