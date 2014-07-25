struct X {
   vector<int> a, b;
   double x, y; // yay
};
[[out]]--------------------------------------------------
Program{
   StructDecl(id:'X', {
      DeclStmt(Type(Template(id:'vector', Args = {Type(id:'int')})), Vars = {"a", "b"})
      DeclStmt(Type(id:'double'), Vars = {"x", "y"})
   })
}
[[err]]--------------------------------------------------
