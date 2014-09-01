int f ( ) {
   vector<int> v(3, 1);
}
[[out]]--------------------------------------------------
Program{
   FuncDecl(id:'f', Type(id:'int'), Params = {}, {
      Block({
         DeclStmt(Type(id:'vector'<Type(id:'int')>), Vars = {"v"(Args = {Int<3>, Int<1>})})
      })
   })
}
[[err]]--------------------------------------------------
