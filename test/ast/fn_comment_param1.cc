int main(int a  ,/* bla */  int    b   )  {}
[[out]]------------------------------------
Program{
   FuncDecl("main", Type(int), Params = {"a": Type(int), "b": Type(int)}, {
      Stmt(block, {})
   })
}
[[err]]------------------------------------
