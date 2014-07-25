struct X {
   int a, b;
};
[[out]]--------------------------------------------------
Program{
   StructDecl(id:'X', {
      DeclStmt(Type(id:'int'), Vars = {"a", "b"})
   })
}
[[err]]--------------------------------------------------
