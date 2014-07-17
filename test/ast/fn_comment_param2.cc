int main(int a  , int   /* bla */ b   )  {}
[[out]]------------------------------------
Program{
   FuncDecl("main", Type(int), Params = {"a": Type(int), "b": Type(int)}, {
      Stmt(block, {})
   })
}
[[err]]------------------------------------
