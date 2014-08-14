struct Blah {
   int x, y;
};

const Bleh& f();
[[out]]----------------------------------------
Program{
   StructDecl(id:'Blah', {
      DeclStmt(Type(id:'int'), Vars = {"x", "y"})
   })
   FuncDecl("f", Type<&>(id:'Bleh', {const}), Params = {})
}
[[err]]----------------------------------------
