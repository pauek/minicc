void f() {
   std::cout << "hola" << std::endl;
}
[[out]]------------------------------------
Program{
   FuncDecl(id:'f', Type(id:'void'), Params = {}, {
      Block({
         ExprStmt(<<(<<(id:[id:'std']'cout', String<hola>), id:[id:'std']'endl'))
      })
   })
}
