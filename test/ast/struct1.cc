struct X {
   int a, b;
};
[[out]]--------------------------------------------------
Program{
   StructDecl('X', {
      DeclStmt(Type(id:'int'), Vars = {"a", "b"})
   })
}
