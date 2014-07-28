int f ( ) {
   vector<int> v(3, 1);
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(id:'int'), Params = {}, {
      Block({
         DeclStmt(Type(Template(id:'vector', Args = {Type(id:'int')})), Vars = {"v"(Args = {Int<3>, Int<1>})})
      })
   })
}
[[err]]--------------------------------------------------
